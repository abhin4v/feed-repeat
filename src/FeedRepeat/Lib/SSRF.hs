module FeedRepeat.Lib.SSRF (checkPublicUrl, isPrivateIPv4, isPrivateIPv6) where

import Control.Exception (SomeException, try)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char (toLower)
import Data.Word (Word32, Word8)
import Net.IPv4 qualified as IPv4
import Network.Socket (AddrInfo (..), SockAddr (..), defaultHints, getAddrInfo, hostAddressToTuple)
import Network.URI qualified as URI
import System.Timeout (timeout)

dnsTimeoutMicroseconds :: Int
dnsTimeoutMicroseconds = 10_000_000 -- 10 seconds

-- | SSRF check: resolves the host via DNS and checks
-- resolved IPs against private/reserved address ranges.
-- Returns False if the host is private, DNS fails, or the URL is invalid.
checkPublicUrl :: String -> IO Bool
checkPublicUrl url = case URI.parseURI url >>= URI.uriAuthority of
  Nothing -> return False -- invalid URL
  Just auth -> do
    let host = URI.uriRegName auth
    if isPrivateName host
      then return False -- hostname is a private name (localhost etc)
      else
        timeout dnsTimeoutMicroseconds (try $ getAddrInfo (Just defaultHints) (Just host) Nothing) >>= \case
          Nothing -> return False -- DNS lookup timed out
          Just (Left (_ :: SomeException)) -> return False -- DNS lookup failed
          Just (Right []) -> return False -- no addresses resolved
          Just (Right addrs) -> return $ not $ any (isPrivateSockAddr . addrAddress) addrs

isPrivateName :: String -> Bool
isPrivateName host =
  map toLower host
    `elem` [ "localhost",
             "localhost.localdomain",
             "localhost6",
             "ip6-localhost",
             "[::1]",
             "127.0.0.1",
             "0.0.0.0"
           ]

isPrivateSockAddr :: SockAddr -> Bool
isPrivateSockAddr = \case
  SockAddrInet _ addr -> isPrivateIPv4 $ hostAddressToTuple addr
  SockAddrInet6 _ _ (w1, w2, w3, w4) _ -> isPrivateIPv6 w1 w2 w3 w4
  _ -> False

isPrivateIPv4 :: (Word8, Word8, Word8, Word8) -> Bool
isPrivateIPv4 (a, b, c, d) = IPv4.reserved $ IPv4.ipv4 a b c d

isPrivateIPv6 :: Word32 -> Word32 -> Word32 -> Word32 -> Bool
isPrivateIPv6 w1 w2 w3 w4 =
  w1 == 0 && w2 == 0 && w3 == 0 -- ::/96 (unspecified, loopback, deprecated IPv4-compatible)
    || (w1 .&. 0xffc00000) == 0xfe800000 -- fe80::/10 (link-local)
    || (w1 .&. 0xfe000000) == 0xfc000000 -- fc00::/7 (ULA)
    || (w1 .&. 0xff000000) == 0xff000000 -- ff00::/8 (multicast)
    || (w1 .&. 0xffc00000) == 0xfec00000 -- fec0::/10 (site-local deprecated)
    || any isPrivateIPv4 (embeddedIPv4 w1 w2 w3 w4)

-- | Extract the embedded IPv4 from an address that uses one of the
-- IPv4-in-IPv6 encoding schemes. Returns every IPv4 value extracted
-- by any matching scheme; the caller decides what to do with the list.
--
-- Schemes recognised:
--   * @::ffff:0:0/96@ — IPv4-mapped (RFC 4291)
--   * @64:ff9b::/96@ — NAT64 well-known prefix (RFC 6052)
--   * @2002::/16@     — 6to4 (RFC 3056)
--   * @2001:0::/32@   — Teredo (RFC 4380); client IPv4 is @w4 XOR 0xFFFFFFFF@
--   * ISATAP          — interface ID @0000:5efe:IPv4@, @0200:5efe:IPv4@, @4000:5efe:IPv4@, or @4200:5efe:IPv4@ (RFC 5214 §6.1, RFC 6964)
embeddedIPv4 :: Word32 -> Word32 -> Word32 -> Word32 -> [(Word8, Word8, Word8, Word8)]
embeddedIPv4 w1 w2 w3 w4 =
  let isatapMarkers = [0x00005efe, 0x02005efe, 0x40005efe, 0x42005efe]
      isatap = [netOrderToTuple w4 | w3 `elem` isatapMarkers]
      ipv4Mapped = [netOrderToTuple w4 | w1 == 0 && w2 == 0 && w3 == 0xffff]
      nat64 = [netOrderToTuple w4 | w1 == 0x0064ff9b && w2 == 0 && w3 == 0]
      sixToFour = [netOrderToTuple sixToFourIPv4 | (w1 .&. 0xffff0000) == 0x20020000]
      teredo = [netOrderToTuple (w4 `xor` 0xffffffff) | w1 == 0x20010000]
   in isatap ++ ipv4Mapped ++ nat64 ++ sixToFour ++ teredo
  where
    sixToFourIPv4 = ((w1 .&. 0xffff) `shiftL` 16) .|. (w2 `shiftR` 16)
    netOrderToTuple w =
      let byte i = fromIntegral (w `shiftR` i) :: Word8
       in (byte 24, byte 16, byte 8, byte 0)
