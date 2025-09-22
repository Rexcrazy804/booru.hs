{
  mkDerivation,
  base,
  lib,
  toml-parser,
  template-haskell,
  text,
  hspec,
  hspec-discover,
  aeson,
  containers,
  hashable,
  http-conduit,
  vector,
  aeson-qq,
  bytestring,
  directory,
  split,
  filepath,
  optparse-applicative_0_19_0_0,
}: let
  inherit (lib.fileset) toSource unions fileFilter;
  hsfilter = fileFilter (file: lib.any file.hasExt ["hs"]);
  root = ../.;

  src = toSource {
    inherit root;
    fileset = unions [
      (hsfilter (root + /app))
      (hsfilter (root + /lib))
      (hsfilter (root + /lib-cli))
      (hsfilter (root + /test))
      (root + /examples/config.toml)
      (root + /booru-hs.cabal)
      (root + /CHANGELOG.md)
      (root + /LICENSE)
    ];
  };
in
  mkDerivation {
    inherit src;
    pname = "booru-hs";
    version = "0.0.1.0";
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [
      aeson
      base
      bytestring
      containers
      hashable
      http-conduit
      split
      text
      toml-parser
      vector
    ];
    executableHaskellDepends = [
      base
      bytestring
      containers
      directory
      filepath
      optparse-applicative_0_19_0_0
    ];
    testHaskellDepends = [
      aeson
      aeson-qq
      base
      containers
      hspec
      template-haskell
      text
      toml-parser
    ];
    testToolDepends = [hspec-discover];
    homepage = "https://github.com/Rexcrazy804/booru.hs";
    description = "Extensible Auto-categorizing image library";
    license = lib.licenses.mpl20;
    mainProgram = "booru-hs";
  }
