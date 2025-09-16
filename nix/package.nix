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
}: let
  inherit (lib.fileset) toSource unions fileFilter;
  hsfilter = fileFilter (file: lib.any file.hasExt ["hs"]);
  root = ../.;

  src = toSource {
    inherit root;
    fileset = unions [
      (hsfilter (root + /app))
      (hsfilter (root + /lib))
      (hsfilter (root + /test))
      (root + /booru-hs.cabal)
      (root + /CHANGELOG.md)
      (root + /LICENSE)
    ];
  };
in
  mkDerivation {
    inherit src;
    # some tests require internet connection
    doCheck = false;
    pname = "booru-hs";
    version = "0.0.1.0";
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [
      aeson
      base
      containers
      hashable
      http-conduit
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
    homepage = "https://github.com/Rexcrazy804/booru.hs";
    description = "Extensible Auto-categorizing image library";
    license = lib.licenses.mpl20;
    mainProgram = "booru-hs";
  }
