{
  mkDerivation,
  base,
  lib,
  toml-parser,
  template-haskell,
  text,
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
    pname = "booru-hs";
    version = "0.0.0.1";
    isLibrary = true;
    isExecutable = true;
    libraryHaskellDepends = [base template-haskell toml-parser text];
    executableHaskellDepends = [base];
    testHaskellDepends = [base];
    homepage = "https://github.com/Rexcrazy804/booru.hs";
    description = "Extensible Auto-categorizing image library";
    license = lib.licenses.mpl20;
    mainProgram = "booru-hs";
  }
