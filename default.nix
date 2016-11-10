with { pkgs = import <nixpkgs> {}; };

with { inherit (pkgs) clangStdenv autoconf-archive autoreconfHook; };

clangStdenv.mkDerivation rec {
  name = "nixexpr-0.0.1";

  src = ./.; # don't nix-build into current directory!

  buildInputs = [ autoconf-archive autoreconfHook ];
}
