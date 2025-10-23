{
  mkDerivation,
  aeson,
  aeson-qq,
  base,
  bytestring,
  containers,
  directory,
  filepath,
  hashable,
  hspec,
  hspec-discover,
  http-client,
  http-conduit,
  lib,
  network-uri,
  optparse-applicative_0_19_0_0,
  split,
  template-haskell,
  text,
  toml-parser,
  vector,
}: let
  inherit (lib.fileset) toSource unions fileFilter;
  hsfilter = fileFilter (file: lib.any file.hasExt ["hs"]);
  root = ../.;

  src = toSource {
    inherit root;
    fileset = unions [
      (hsfilter (root + /app))
      (hsfilter (root + /lib))
      (hsfilter (root + /booru-cli))
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
    version = "1.0.1.2";
    isLibrary = true;
    isExecutable = true;
    enableSeparateDataOutput = true;
    libraryHaskellDepends = [
      aeson
      base
      bytestring
      containers
      directory
      filepath
      hashable
      http-client
      http-conduit
      network-uri
      optparse-applicative_0_19_0_0
      split
      text
      toml-parser
      vector
    ];
    executableHaskellDepends = [base];
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
    doHaddock = false;
    homepage = "https://github.com/Rexcrazy804/booru.hs";
    description = "Extensible Auto-categorizing image library";
    license = lib.licenses.mpl20;
    mainProgram = "booru-hs";
  }
