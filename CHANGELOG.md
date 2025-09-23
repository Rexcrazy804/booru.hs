# Revision history for booru-hs

## 1.0.0.0 -- 2025-09-23
Stable Release!!
further releases will follow https://pvp.haskell.org/#decision-tree

- Established CLI interface
- Established unified config structure (see examples/config.toml)
- genCategory now categorizes images with empty tags into "unknown"
- previews now utilize id over resolvedName
- changes to resolvedName spec (removal of '|' separator)
- created Parser module
- created requestfile function for downloads
- CI now builds the default package as part of tests

Last Revision
- [480ea3b](https://github.com/Rexcrazy804/booru.hs/tree/480ea3b9c51302c359047ed30f6a1868ef9dd13d) feat(cli): download subcommand now preserves file extension

## 0.0.4.0 -- 2025-09-20
- Addition of Preview generation
- Decoupling of Fitlers and Previews from Sources
- Sepation of Filters into Filter and PFilter
- Relocated processing functions into Core
- Relocated Tags and Identifiers under Images
- PFilters now supports provider filtering

Last Revision
- [f840d5c](https://github.com/Rexcrazy804/booru.hs/tree/f840d5c74cd0a67c834a03608e7ffa4c9b9bf607) feat(tests): added tests for category filtering

## 0.0.3.0 -- 2025-09-19
- Addition of Core categorization module (MILESTONE)
- Support for Synonym Processing
- SUpport for applying Overrides
- Correct hashing and Derriation of unique resolveName (IMP)
- added explicit styling with fourmolu

Last Revision
- [4993ebb](https://github.com/Rexcrazy804/booru.hs/tree/4993ebbd195313fd9fd090ae9b8509b13eb4a71f) feat(tests): added tests for categorization

## 0.0.2.0 -- 2025-09-17
- Addition of networking infrastrucutre
- Addition of builtin providers
- Initialization of Image strucutre
- Addition of Dynamic User-Agent header

Last Revision
- [afe8666](https://github.com/Rexcrazy804/booru.hs/tree/a54a8e6cf77b2fe2aeb8c559c5c1571840f6d056) docs: correctly order changelog in decending order of recency

## 0.0.1.0 -- 2025-09-15

Initial Structures for the following
- Parser Schema
- Nix infrastructure
- Test Suite

Last Revision
- [a54a8e6](https://github.com/Rexcrazy804/booru.hs/tree/a54a8e6cf77b2fe2aeb8c559c5c1571840f6d056) test: extended providers test with multi provider test

## 0.0.0.1 -- 2025-09-13

* First version. Released on an unsuspecting world.
