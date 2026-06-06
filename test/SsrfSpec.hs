module SsrfSpec (ssrfSpec) where

import FeedRepeat.Lib.SSRF (checkPublicUrl, isPrivateIPv6)
import Test.Hspec

ssrfSpec :: Spec
ssrfSpec = do
  describe "isPrivateIPv6" $ do
    it "rejects ::/96 (unspecified, loopback, deprecated IPv4-compat)" $ do
      isPrivateIPv6 0x00000000 0x00000000 0x00000000 0x00000000 `shouldBe` True -- ::
      isPrivateIPv6 0x00000000 0x00000000 0x00000000 0x00000001 `shouldBe` True -- ::1 (loopback)
      isPrivateIPv6 0x00000000 0x00000000 0x00000000 0xc0a80101 `shouldBe` True -- ::192.168.1.1 (deprecated IPv4-compat)
    it "allows 1:: (distinct from ::1)" $ do
      isPrivateIPv6 0x00000001 0x00000000 0x00000000 0x00000000 `shouldBe` False -- 1:: is public
    it "rejects fe80::/10 (link-local)" $ do
      isPrivateIPv6 0xfe800000 0x00000000 0x00000000 0x00000000 `shouldBe` True -- fe80::
      isPrivateIPv6 0xfebf0000 0x00000000 0x00000000 0x00000000 `shouldBe` True -- febf:: (upper bound)
    it "rejects fc00::/7 (unique local / ULA)" $ do
      isPrivateIPv6 0xfc000000 0x00000000 0x00000000 0x00000000 `shouldBe` True -- fc00::
      isPrivateIPv6 0xfd000000 0x00000000 0x00000000 0x00000000 `shouldBe` True -- fd00::
      isPrivateIPv6 0xfdffffff 0x00000000 0x00000000 0x00000000 `shouldBe` True -- fdff:ffff::
    it "rejects ff00::/8 (multicast)" $ do
      isPrivateIPv6 0xff000000 0x00000000 0x00000000 0x00000000 `shouldBe` True -- ff00::
      isPrivateIPv6 0xff020000 0x00000000 0x00000000 0x00000000 `shouldBe` True -- ff02::1 (all-nodes)
      isPrivateIPv6 0xffffffff 0x00000000 0x00000000 0x00000000 `shouldBe` True -- ffff::
    it "rejects fec0::/10 (site-local deprecated)" $ do
      isPrivateIPv6 0xfec00000 0x00000000 0x00000000 0x00000000 `shouldBe` True -- fec0::
      isPrivateIPv6 0xfee00000 0x00000000 0x00000000 0x00000000 `shouldBe` True -- fee0:: (inside fec0::/10 range)
    it "accepts public IPv6 ranges" $ do
      isPrivateIPv6 0x20010db8 0x00000000 0x00000000 0x00000000 `shouldBe` False -- 2001:db8:: (documentation)
      isPrivateIPv6 0x20010000 0x00000000 0x00000000 0xf7f7f7f7 `shouldBe` False -- 2001:: Teredo, client 8.8.8.8 (XOR'd)
      isPrivateIPv6 0x24040000 0x00000000 0x00000000 0x00000000 `shouldBe` False -- 2404:: (public)
      isPrivateIPv6 0xfe000000 0x00000000 0x00000000 0x00000000 `shouldBe` False -- fe00:: (unassigned, not private)
    it "rejects IPv4-mapped ::ffff:0:0/96 with private embedded IPv4" $ do
      isPrivateIPv6 0x00000000 0x00000000 0x0000ffff 0xc0a80101 `shouldBe` True -- ::ffff:192.168.1.1
      isPrivateIPv6 0x00000000 0x00000000 0x0000ffff 0x0a000001 `shouldBe` True -- ::ffff:10.0.0.1
      isPrivateIPv6 0x00000000 0x00000000 0x0000ffff 0x7f000001 `shouldBe` True -- ::ffff:127.0.0.1
      isPrivateIPv6 0x00000000 0x00000000 0x0000ffff 0x00000000 `shouldBe` True -- ::ffff:0.0.0.0
      isPrivateIPv6 0x00000000 0x00000000 0x0000ffff 0xffffffff `shouldBe` True -- ::ffff:255.255.255.255 (broadcast)
      isPrivateIPv6 0x00000000 0x00000000 0x0000ffff 0xc6120001 `shouldBe` True -- ::ffff:198.18.0.1 (benchmark)
    it "allows IPv4-mapped ::ffff:0:0/96 with public embedded IPv4" $ do
      isPrivateIPv6 0x00000000 0x00000000 0x0000ffff 0x08080808 `shouldBe` False -- ::ffff:8.8.8.8
      isPrivateIPv6 0x00000000 0x00000000 0x0000ffff 0x01020304 `shouldBe` False -- ::ffff:1.2.3.4
    it "rejects NAT64 64:ff9b::/96 with private embedded IPv4" $ do
      isPrivateIPv6 0x0064ff9b 0x00000000 0x00000000 0xc0a80101 `shouldBe` True -- 64:ff9b::192.168.1.1
      isPrivateIPv6 0x0064ff9b 0x00000000 0x00000000 0x0a000001 `shouldBe` True -- 64:ff9b::10.0.0.1
      isPrivateIPv6 0x0064ff9b 0x00000000 0x00000000 0xc6120001 `shouldBe` True -- 64:ff9b::198.18.0.1 (benchmark)
    it "allows NAT64 64:ff9b::/96 with public embedded IPv4" $ do
      isPrivateIPv6 0x0064ff9b 0x00000000 0x00000000 0x08080808 `shouldBe` False -- 64:ff9b::8.8.8.8
      isPrivateIPv6 0x0064ff9b 0x00000000 0x00000000 0x01020304 `shouldBe` False -- 64:ff9b::1.2.3.4
    it "rejects 6to4 2002::/16 with private embedded IPv4" $ do
      isPrivateIPv6 0x2002c0a8 0x01010000 0x00000000 0x00000000 `shouldBe` True -- 2002:c0a8:0101:: (192.168.1.1)
      isPrivateIPv6 0x20020a00 0x00010000 0x00000000 0x00000000 `shouldBe` True -- 2002:0a00:0001:: (10.0.0.1)
      isPrivateIPv6 0x20027f00 0x00010000 0x00000000 0x00000000 `shouldBe` True -- 2002:7f00:0001:: (127.0.0.1)
    it "allows 6to4 2002::/16 with public embedded IPv4" $ do
      isPrivateIPv6 0x20020808 0x08080000 0x00000000 0x00000000 `shouldBe` False -- 2002:0808:0808:: (8.8.8.8)
      isPrivateIPv6 0x20020102 0x03040000 0x00000000 0x00000000 `shouldBe` False -- 2002:0102:0304:: (1.2.3.4)
    it "rejects 6to4 2002::/16 with benchmark-range embedded IPv4" $ do
      isPrivateIPv6 0x2002c612 0x13000000 0x00000000 0x00000000 `shouldBe` True -- 2002:c612:1300:: (198.18.19.0, benchmark)
    it "does not falsely reject near-miss 6to4 prefixes" $ do
      isPrivateIPv6 0x20030000 0x00000000 0x00000000 0x00000000 `shouldBe` False -- 2003:: (public, not 2002::)
      isPrivateIPv6 0x2001ffff 0x00000000 0x00000000 0x00000000 `shouldBe` False -- 2001:ffff:: (public, not 2002::)
    it "rejects Teredo 2001:0::/32 with private (XOR'd) embedded IPv4" $ do
      -- 192.168.1.1 = 0xC0A80101; XOR 0xFFFFFFFF = 0x3F57FEFE
      isPrivateIPv6 0x20010000 0x12345678 0xffff1234 0x3f57fefe `shouldBe` True
      -- 10.0.0.1 = 0x0A000001; XOR 0xFFFFFFFF = 0xF5FFFFFE
      isPrivateIPv6 0x20010000 0x9abcdef0 0x80000000 0xf5fffffe `shouldBe` True
      -- 127.0.0.1 = 0x7F000001; XOR 0xFFFFFFFF = 0x80FFFFFE
      isPrivateIPv6 0x20010000 0x00000000 0x00000000 0x80fffffe `shouldBe` True
      -- 198.18.0.1 = 0xC6120001; XOR 0xFFFFFFFF = 0x39EDFFFE
      isPrivateIPv6 0x20010000 0x00000000 0x00000000 0x39edfffe `shouldBe` True
    it "allows Teredo 2001:0::/32 with public (XOR'd) embedded IPv4" $ do
      -- 8.8.8.8 = 0x08080808; XOR 0xFFFFFFFF = 0xF7F7F7F7
      isPrivateIPv6 0x20010000 0x00000000 0xffff0000 0xf7f7f7f7 `shouldBe` False
      -- 1.1.1.1 = 0x01010101; XOR 0xFFFFFFFF = 0xFEFEFEFE
      isPrivateIPv6 0x20010000 0x12345678 0x00000000 0xfefefefe `shouldBe` False
    it "rejects ISATAP with private embedded IPv4 (0000:5efe form)" $ do
      isPrivateIPv6 0x20010db8 0x00000000 0x00005efe 0xc0a80101 `shouldBe` True -- 192.168.1.1
      isPrivateIPv6 0x20010db8 0x00000000 0x00005efe 0x0a000001 `shouldBe` True -- 10.0.0.1
      isPrivateIPv6 0xfe800000 0x00000000 0x00005efe 0x7f000001 `shouldBe` True -- 127.0.0.1 (link-local ISATAP)
    it "rejects ISATAP with private embedded IPv4 (0200:5efe form, u-bit set)" $ do
      isPrivateIPv6 0x20010db8 0x00000000 0x02005efe 0xc0a80101 `shouldBe` True
      isPrivateIPv6 0xfe800000 0x00000000 0x02005efe 0x0a000001 `shouldBe` True
    it "allows ISATAP with public embedded IPv4" $ do
      isPrivateIPv6 0x20010db8 0x00000000 0x00005efe 0x08080808 `shouldBe` False -- 8.8.8.8
      isPrivateIPv6 0x20010db8 0x00000000 0x02005efe 0x01020304 `shouldBe` False -- 1.2.3.4
    it "rejects ISATAP with benchmark-range embedded IPv4" $ do
      isPrivateIPv6 0x20010db8 0x00000000 0x00005efe 0xc6121300 `shouldBe` True -- 198.18.19.0
    it "rejects ISATAP with private embedded IPv4 (4000:5efe form, g-bit set)" $ do
      isPrivateIPv6 0x20010db8 0x00000000 0x40005efe 0xc0a80101 `shouldBe` True -- 192.168.1.1
      isPrivateIPv6 0x20010db8 0x00000000 0x40005efe 0x0a000001 `shouldBe` True -- 10.0.0.1
      isPrivateIPv6 0xfe800000 0x00000000 0x40005efe 0x7f000001 `shouldBe` True -- 127.0.0.1 (link-local ISATAP)
      isPrivateIPv6 0x20010db8 0x00000000 0x40005efe 0xc6120001 `shouldBe` True -- 198.18.0.1 (benchmark)
    it "rejects ISATAP with private embedded IPv4 (4200:5efe form, g-bit + u-bit set)" $ do
      isPrivateIPv6 0x20010db8 0x00000000 0x42005efe 0xc0a80101 `shouldBe` True -- 192.168.1.1
      isPrivateIPv6 0xfe800000 0x00000000 0x42005efe 0x0a000001 `shouldBe` True -- 10.0.0.1
      isPrivateIPv6 0x20010db8 0x00000000 0x42005efe 0x7f000001 `shouldBe` True -- 127.0.0.1
      isPrivateIPv6 0x20010db8 0x00000000 0x42005efe 0xc6120001 `shouldBe` True -- 198.18.0.1 (benchmark)
    it "allows ISATAP with public embedded IPv4 (g-bit forms)" $ do
      isPrivateIPv6 0x20010db8 0x00000000 0x40005efe 0x08080808 `shouldBe` False -- 8.8.8.8
      isPrivateIPv6 0x20010db8 0x00000000 0x42005efe 0x01020304 `shouldBe` False -- 1.2.3.4
    it "does not falsely match near-miss ISATAP markers" $ do
      isPrivateIPv6 0x20010db8 0x00000000 0x00015efe 0xc0a80101 `shouldBe` False -- wrong 2nd byte
      isPrivateIPv6 0x20010db8 0x00000000 0x00005eff 0xc0a80101 `shouldBe` False -- 5eff not 5efe
      isPrivateIPv6 0x20010db8 0x00000000 0x00005ef0 0xc0a80101 `shouldBe` False -- trailing zero
    it "rejects addresses matching both 6to4 and ISATAP when 6to4 IPv4 is private" $ do
      -- 6to4: 192.168.1.1 (private); ISATAP: 8.8.8.8 (public) — must flag as private
      isPrivateIPv6 0x2002c0a8 0x01010000 0x00005efe 0x08080808 `shouldBe` True
      -- 6to4: 10.0.0.1 (private); ISATAP: 1.1.1.1 (public)
      isPrivateIPv6 0x20020a00 0x00010000 0x02005efe 0x01010101 `shouldBe` True
    it "rejects addresses matching both 6to4 and ISATAP when ISATAP IPv4 is private" $ do
      -- 6to4: 8.8.8.8 (public); ISATAP: 192.168.1.1 (private)
      isPrivateIPv6 0x20020808 0x08080000 0x00005efe 0xc0a80101 `shouldBe` True
      -- 6to4: 1.1.1.1 (public); ISATAP: 10.0.0.1 (private)
      isPrivateIPv6 0x20020101 0x01010000 0x00005efe 0x0a000001 `shouldBe` True
    it "allows addresses matching both 6to4 and ISATAP when both are public" $ do
      -- 6to4: 8.8.8.8 (public); ISATAP: 1.1.1.1 (public)
      isPrivateIPv6 0x20020808 0x08080000 0x00005efe 0x01010101 `shouldBe` False
    it "does not falsely match near-miss Teredo prefix" $ do
      isPrivateIPv6 0x20010001 0x00000000 0x00000000 0x3f57fefe `shouldBe` False -- 2001:1::, not 2001:0::
      isPrivateIPv6 0x2000ffff 0x00000000 0x00000000 0x3f57fefe `shouldBe` False -- 2000:ffff::, not 2001:0::
      --
  describe "checkPublicUrl (DNS-resolving check)" $ do
    it "rejects localhost" $ do
      result <- checkPublicUrl "http://localhost/feed"
      result `shouldBe` False

    it "rejects 127.0.0.1 via DNS resolution" $ do
      result <- checkPublicUrl "http://127.0.0.1/feed"
      result `shouldBe` False

    it "accepts public IP literal (no DNS required)" $ do
      result <- checkPublicUrl "http://8.8.8.8/feed"
      result `shouldBe` True
