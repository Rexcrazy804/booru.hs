# Booru-hs | Extensible Booru Library

Booru-hs is a piece of software that allows you to categorize
your images from various booru sources into folders of

- Artists
- Copyrights
- Characters

Each folder contains subfolders for each available value of the attribute,
and each subfolder contains images belonging to said attribute value.
The following image should clarify its capabilities

<img width="1920" height="1080" alt="image" src="https://github.com/user-attachments/assets/80d528c8-5776-4ddc-96e3-648cf6a0ca12" />

- foreground: [Kokomi by soraneko hino][fg-art-source]
- background: [Kokomi by omces96][bg-art-source]

## Table of Contents

- [Features](#features)
- [Installation](#installation)
  - [NixOS](#nixos-installation)
  - [other operating systems](#non-nix-installtion)
- [Configuration](#configuration)
- [Usage](#usage)
- [Specifications](#specifications)
  - [Sources](#source-specification)
  - [Providers](#provider-specification)
  - [Filters](#filters-specification)
  - [Preview filters](#preview-filters-specification)
  - [Synonyms](#synonyms-specification)
- [Acknowledgements](#Acknowledgements)
- [Licensing](#Licensing)

## Features

The following are the additions booru-hs provider over [booru-flake]
- Extensible Providers: you are no longer limited to danbooru,
  any image board with a GET API can that responds with a json can be used
- Overrides: Overrides enable appending or overwriting information fetched from providers
- Synonyms: Allows coercing a list of tags to a singular tag [see the note here](examples/synonyms.toml)

## Installation

You may build the project from source using cabal,
but the preferred method is to use the exported package in the flake

### NixOS installation

Add this repository as a flake
```nix
{
  inputs = {
    booru-hs = {
      url = "github:Rexcrazy804/booru.hs";
      inputs.nixpkgs.follows = "nixpkgs";
      # optional (if applicable)
      # inputs.systems.follows = "systems";
    };
  };
}
```

Then, add the default package to your `systemPackages` or `users.users.${username}.packages` list
```nix
{inputs, pkgs, ...}: {
  environment.systemPackages = [(inputs.booru-hs.packages.${pkgs.system}.default)];
}
```

Switch to the new configuration and run `booru-hs gen-config` this will generate a config at
`$XDG_CONFIG_HOME/booru/config.toml` check it out and head to [Configuration](#configuration)
or [Usage](#usage) sections

### Non nix installation

> TODO <br>
> Technically you should be able to install this on any distribution with cabal.
> Potentially, even windows .w.

## Configuration

Booru-hs is configured using a toml file at `$XDG_CONFIG_HOME/booru/config.toml`
A very minimal configuration will look like the following
```toml
[[sources]]
provider = "danbooru"
ids = ["5614777", "9590836", "9561442"]
```

An exhaustive list of options is provided by default with booru-hs
and can be generated using
```sh
booru-hs gen-config
```

The same can also be found at [examples/config.toml](examples/config.toml).
The full potential of borou-hs relies on this configuration file,
that exposes various attributes that can be leveraged to extend booru-hs,
and alter its categorization behaviour

## Usage

The booru-hs cli equiped with help descriptions for every option.
You may invoke these using `--help` or `<subcommand> --help` for subcommand info.

### Build subcommand

This is the bread and butter for booru-hs.
Once the configuration is generated, it can be invoked with
```sh
booru-hs build
```

This will do a couple of things for the first run
- Download metadata for given ids from requested providers
- Download the actual image files
- Build the categorized image folder

Subsequent runs will just perform the categorization step,
efficiently downloading and querying metadata for newer ids only.

The raw images and data are stored in `$XDG_DATA_HOME/booru`

### Query subcommand

```sh
booru-hs query <TAGS>
```

The query subcommand can be used to quickly build a folder
containing images that contain a given list of tags.

## Specifications

A complete set of commented toml files describing the specification
for various attributes of the configuration

### Source specification

```toml
[[sources]]
# name of provider, builtin providers: danbooru, urls
provider = "myprovider"
# list of ids to be fetched using the provider
# ids can be either "<id>" or "<id> <nickname>"
ids = ["12 kok", "13", "14"]

[[sources.overrides]]
# an id or nickname
# nicknames don't have to be unique
# they can be used to apply the same override
# across multiple images
identifier = "kok"
# overwrites information if false
# otherwise adds information
append = true
# all fields are valid, regardless of the provider
# this is great for the `urls` provider which adds no extra information
characters = ["kokomi"]
copyrights = ["genshin_impact"]
tags = ["jellyfish", "underwater"]
rating = "explicit"
```

### Provider specification

```toml
[[providers]]
# the name must be unique
# overlapping names will overwrite the previous instance
name = "danbooru"
# The url MUST contain %%ID%%
# the id's from the sources will be substituted there
# (hey now that I think about it there is nothing in the
# program that prohibits you from doing so .w.)
url = "https://danbooru.donmai.us/posts/%%ID%%.json"

# NOTE
# the following fields are attribute values
# returned by the metadata json
# so file = "file_url" implies that
# from the json danbooru responds with
# .file_url will contain the file
#
# this is a soft requirement
# providers without this will be considered "special"
# special providers internally skip the metadata fetch phase
# and its url will be be substituted to download the file
file = "file_url"
# contains the thumbnail sized image
# which will be utilized to generate previews.md
preview_file = "preview_file_url"
# rest are self explanatory
artists = "tag_string_artist"
characters = "tag_string_character"
copyrights = "tag_string_copyright"
tags = "tag_string_general"
rating = "rating"

[[providers]]
# A special builtin provider
name = "urls"
# accepts a list of urls in the id's field and simply
# downloads them skipping the metadata fetch
url = "%%ID%%"
# values prefixed with a $ is inferred as the default value
# for that field for the given provider
rating = "$general"
# space separated values are automatically inferred as a list
copyrights = "$copyright1 copyright2 copyright3"
```

### Filters specification

```toml
[filters]
# a filter contains two attributes
# list:       a list of strings where each element is a tag
#             to be filtered
# inverted:   determines whether the values are filter out
characters = { list = ["kokomi"], inverted = true }
copyrights = { list = ["redarchive"], inverted = false }
artists = { list = ["mourncolor", "elodias"], inverted = true }

# these filters act on the resulting sub-directories
# generated by the build sub command
```

### Preview filters specification

```toml
# same as in filters' specification
# just that these act on the entirety of the generated
# preview.md
[preview_filters]
characters = { list = ["abc", "xyz"], inverted = false }
copyrights = { list = ["arknights"], inverted = false }
artists = { list = ["mourncolor", "elodias"], inverted = false }
tags = { list = ["bird", "horse"], inverted = false }
ids = { list = ["11112"], inverted = false }                                
ratings = { list = ["g"], inverted = true }                                 
providers = { list = ["s34"], inverted = false}
```

### synonyms-specification

```toml
# synonyms simply group several identical tags into a common tag
# for the purpose of unifying different conventions
# used across various booru providers

# use of synonyms help us defragment our file structure
# from running into issues like the following

# characters/
# |- kokomi/
# ||- XXX1.png
# |- sangonomiya_kokomi/
# ||- YYY1.png
# |- kokomi sangonomiya/
# ||- ZZZ1.png

# clearly those are the same characters,
# but due the conventions used in their differing sources,
# they were separated into different folders.

# If we configured our synonyms to say
# [synonyms.character] 
# kokomi = ["sangonomiya_kokomi", "kokomi sangonomiya"]
# we will then have a cleaner structure as follows

# characters/
# |- kokomi/
# ||- XXX1.png
# ||- YYY1.png
# ||- ZZZ1.png

# each table is a flexible key value map
[synonyms.characters]
# <unified tag> = [<list of synonyms>]
sangonomiya_kokomi = ["kokomi", "sangonomiya kokomi", "sangonomiya_kokomi_(genshin_impact)"]
yoimiya = ["yoi", "fireworks_girl"]

[synonyms.tags]
girl = ["1girl", "female"]

[synonyms.copyrights]
genshin_impact = ["genshin impact", "yuanshen"]

[synonyms.artists]
elodias = ["little ello"]

[synonyms.ratings]
e = ["explicit", "n$fw"]
```

## Acknowledgements

[toml-parser]

Excellent toml parsing library and served as the foundation for the structure and
tooling for this project.

[booru-flake]

The predecessor to this project, booru-hs is simply an upgrade to booru-flake on most most levels,
I suppose booru-hs is missing a nixosModule (not for long).

[optparse-applicative]

Excellent CLI framework,
besides a few nitpicks its does it job well and is easy to work with.

[conventional-commits]

Specification for commit format.
I should do a better job at using it correctly

[You | the users][stargazers]

Thank you.

## Licensing

All code in this repository are under [MPL-2.0](LICENSE),
unless explicitly stated otherwise.

Creative works, including this readme and other markdown files remain
under [CC-BY-2.0](https://creativecommons.org/licenses/by/2.0/)

All toml files in the examples folder are under [CC0-1.0](https://creativecommons.org/publicdomain/zero/1.0/)

[bg-art-source]: https://danbooru.donmai.us/posts/7240127
[fg-art-source]: https://danbooru.donmai.us/posts/6746291
[stargazers]: https://github.com/Rexcrazy804/booru-flake/stargazers
[booru-flake]: https://github.com/Rexcrazy804/booru-flake
[toml-parser]: https://github.com/glguy/toml-parser
[optparse-applicative]: https://github.com/pcapriotti/optparse-applicative
[conventional-commits]: https://www.conventionalcommits.org/en/v1.0.0/
