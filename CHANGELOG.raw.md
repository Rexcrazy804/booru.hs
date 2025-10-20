# Raw Revision history for booru-hs

## 1.0.1.2 -- 2025-10-19
- feat(cli): added auto tag sub command
- feat(lib): addition of auto tag request function
- feat(cli): added options for build subcommand
- feat(cli): added metadata manipulation subcommand
- feat(cli): build no longer removed redundant image data
- feat(cli): improved getFname' that uses fullname for special providers

## 1.0.0.0 -- 2025-09-23
- feat(cli): download subcommand now preserves file extension
- fix(nix): correctly override optparse with v0_19
- fix(nix): include examples/config.toml
- feat(cli): added ability to generate example config
- feat(cli): exported getCfgFile function in Utils.Common
- feat(workflows): extended tests with a build test
- fix(cli): stored queries in XDGCache
- fix(cli): correct querry algorithm
- feat(cli): added ability to query images into a temporary directory
- feat(lib): exported createTagMap from Booru.Core.Category
- feat(cli): improved filenames for symlinks
- feat(cli): categoryToFs preserves file types
- revert: improved bounds dependencies
- feat: improved bounds dependencies
- fix(nix): include lib-cli
- feat: moved cli into its own internal library
- fix(app): correctly handly maybes
- revert: correctly wrap synonym in maybe for realizeSynonms
- revert: Synonyms now accept Synonyms wrapped in Maybe
- revert: correctly wrap filters in Maybe for filterCatSpec
- revert: filterCategory now accepts Maybe Filter
- revert: correctly wrap fitlers in maybe
- feat(app): relocated null provider
- feat(lib): categorize images with empty tags into 'unknown'
- fix(lib): previews now use id over resolvedName
- fix(nix): update noifd package
- feat(app): support generating previews
- fix(tests): correctly wrap fitlers in maybe
- feat(lib): accept maybe filter in filterImages
- feat(app): build complete
- fix(app): utilized improved common commands
- feat(app): imrpoved common funtions
- feat(app): extended common opts to support plant dir
- feat(app): extended options to include plantdir
- fix(tests): correctly wrap filters in Maybe for filterCatSpec
- feat(lib): filterCategory now accepts Maybe Filter
- feat(lib): minimal loggin for build
- feat(lib): build now efficiently downloads images
- feat(lib): build now processes synonyms
- fix(tests): correctly wrap synonym in maybe for realizeSynonms
- feat(lib): Synonyms now accept Synonyms wrapped in Maybe
- feat(lib): created toResolvedName function
- feat(app): improved cache validation
- feat(lib): derrived ord for Identifier
- fix(app): reflected changes to identifier
- fix(tests): reflected changes to identifier
- fix(lib): correct references to Identifier
- fix(tests): fixed missing import
- fix(app): leveraged parsefile in Cli.Common
- feat(lib): export null provider + used findWithDefault
- feat(lib): rexport encode option in parsers
- feat(lib): created generalized parser
- feat(app): removed xdg-basedir dep
- fix(app): removed default value for --data
- fix(app): ensure FilePath type is preserved
- feat(app): ensure -c and -d args are optional
- fix(app): leveraged common extractCfg + comply with commonOpts
- feat(app): accept common opts in Download subcmd wiht internal parsing of cfg
- feat(app): addition of Cli.Common module
- feat(app): Intial work for cli (#1)
- feat(lib): exported function to parse config
- feat(lib): exported list of builtin providers
- fix(lib): getProvider accepts \[Proivider\] over Providers newtyp
- feat(lib,tests): addition of unified config structure
- feat(lib): added function to request files

## 0.0.4.0 -- 2025-09-20
- feat(tests): added tests for category filtering
- feat(lib): added filter category unit
- fix(tests): reflected relocation of lib files
- fix(lib,tests): corrected Previews references to PFilter refernces
- feat(lib): separated Previews into PFilters
- feat(tests): tests for preview generation and filtering
- feat(lib): addition of preview generation and filtering
- feat(lib): addition of alternate extractId'
- fix(tests): reflect decomposiiton of Filters
- feat(lib): decompose Fitlers into Fitlers and Previews
- feat(tests): reflected addition to Filter
- feat(lib): extended filters to support filtering by provider
- feat(tests): return tests for filters
- fix(tests): reflect removal of filters and previews in source tests
- fix(tests): correct imports to reflect relocation
- fix(lib): correct imports to reflect relocation
- feat(lib): seperation of Filters from Sources
- feat(lib): relocation of tags and identifiers to images

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
