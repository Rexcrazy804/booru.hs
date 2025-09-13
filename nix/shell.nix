{
  taplo,
  jq,
  mkShellNoCC,
}:
mkShellNoCC {
  name = "booru.hs devShell";
  packages = [taplo jq];
}
