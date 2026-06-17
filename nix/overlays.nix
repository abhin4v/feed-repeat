{
  sources,
  compiler,
  static,
  devTools,
}:
[ (final: prev: { inherit (import sources.gitignore { inherit (prev) lib; }) gitignoreFilter; }) ]
++ (if static then import ./overlays-static.nix { inherit compiler; } else [ ])
++ [
  (final: prev: {
    feed-repeat = import ./packages.nix {
      pkgs = if static then final.pkgsMusl else final;
      pkgsOrig = final;
      inherit compiler static devTools;
    };
  })
]
