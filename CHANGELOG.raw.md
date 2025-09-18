# Raw Revision history for booru-hs

## 0.0.3.0 -- 2025-09-19
- feat(tests): added tests for categorization
- feat(lib): addition of CORE categoriztion module
- feat: addition of explicit fourmolu.yaml for styling
- feat(tests): added tests for synonym processing
- feat(lib): addition of synonym processing
- feat(tests): added tests for special providers
- feat(tests): added tests for overrides
- fix(lib): correctly handly overrides for rating
- feat(lib): addition of override application unit
- fix: removed QQ default extension for lib
- fix(tests): reflect resolvedName change in last commit
- fix(lib): correctly derrive resolvedName based on id
- fix(lib): correctly hash the processed url for special providers

## 0.0.2.0 -- 2025-09-17

- feat(tests): added zerochanNet test for requests
- feat(lib): dynamic User-Agent header complying with zerochan
- feat(tests): included zerochanNet to builtin provider integrity test
- feat(lib): added zerochan provider
- feat(lib): added ability to parse array of strings for tags
- feat(tests): extend request tests to include safebooru reqs
- feat(tests): integrity tests for builtin providers
- feat(lib): addition of builtin providers
- feat(workflows): enable online tests
- fix(nix): undo disabling of tests in nix package
- feat(tests): parition online tests
- fix(nix): bump static nix package
- fix(lib): downgrade hashable version
- feat(tests): REMOTE OBJECT PARSING!!!
- fix(lib+test): correctly hash the fileurl for providers with file attr
- feat(lib): initial test for Requests
- feat(lib): created and exported toObject function
- feat(lib): export extractImage and resolveProvider functions
- fix(lib): update RequestProvider's Image struct and logical fixes
- feat(tests): added tests for Image structure
- feat(lib): extended Images to store file(url), preview, and provider information
- fix(lib): correctly derrive generics for Images type
- feat(lib): addition of extractId helper
- fix(lib): correctly use words by for getAttribute helper
- fix: removed non dependencies for executable
- fix(lib): altered Image to hold url instead of id
- feat(lib): addition of Request Provider
- feat(lib): addition of Image schema

## 0.0.1.0 -- 2025-09-15

- feat(lib,tests): addition of structure and tests for synonyms
- fix(spec): simplified synonyms
- feat(lib,tests): extended Sources to support nickname in id fields
- fix(tests): reflect changes to provider
- feat(lib): extend Providers to support default value for attributes
- feat(lib): addition of internal Helper lib
- fix(workflows): use ci-shell in gh workflows
- feat(nix): added minimal ci shell for gh workflows
- fix(workflow): correctly update cabal before executing tests
- fix(workflow): leverage nix for CI
- feat: added generic haskell gh workflow
- fix(app): cleanup main, ensure it can be built
- fix(nix): non ifd derrivation
- fix(tests): correctly mention provider in provider test
- feat(tests): parallel execution of tests
- feat(spec): extended providers spec for default attribute values
- feat: addition of Providers schema
- feat(spec): revamped providers spec + extra providers
- fix(+spec): ensure singularity of rating attribute of overrides
- feat(spec): consolidated synonyms
- feat: Previews attribute reuses filters structure
- fix(spec): seperate previews.filters for previews section
- feat: initalization of test suite
- feat: export Filter in lib
- feat: PARSES SOURCE SPEC!!!!
- fix(spec): correctly pass ids list as strings
- feat(spec): extend synonyms to include ratings
- fix(spec): correct overrides' tags & ratings
- fix(spec): fixed sources.filters and sources.previews from being lists
- feat(spec): improve sources spec
- feat: devShell now uses rapid package
- feat: added ifd package for rapid development
- feat: basic working state for toml-parser lib
- feat: include toml-parser, template-haskel, text dependencies
- feat: populated git ignore
- feat: added haskell package and devshell in nix
- feat: inital haskell strucutre
- fix(spec): renamed override -> overrides
- feat(spec): init synonyms spec
- feat: add early nix infra
- feat(spec): inclusion of nicknames for id fields + relocated sources.sourcename.meta

## 0.0.0.1 -- 2025-09-13

Let there be light
