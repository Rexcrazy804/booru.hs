# Booru-hs | Extensible Booru Library

Booru-hs is a piece of software that allows you to categorize
your images from various booru sources into folders of 

- Aritsts
- Copyrights
- Characters

each folder contains subfolders for each available value of the attribute,
and each subfolder contains images belonging to said attribute value.
The following image should clarify its capabilities

<img width="1920" height="1080" alt="image" src="https://github.com/user-attachments/assets/80d528c8-5776-4ddc-96e3-648cf6a0ca12" />

- foreground: [Kokomi by soraneko hino][fg-art-source]
- background: [Kokomi by omces96][bg-art-source]

## Table of Contents

- [Features](#features)
- [Installation](#installation)
  - [nixos](#nixos-installation)
  - [other operating systems](#non-nix-installtion)
- [Configuration](#configuration)
- [Usage](#usage)
- [Specifications](#specifications)
  - [Sources](#source-spec)
  - [Providers](#provider-spec)
  - [Filters](#filters-spec)
  - [Preview fitlers](#preview-filters-spec)
  - [Synonyms](#synonyms-spec)
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
but the prefered method is to use the exported package in the flake

### Nixos installation

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
> Technically you should be able to install this on any distrobution with cabal.
> Potentially, even windows .w.

## Configuration

Booru-hs is configured using a toml file at `$XDG_CONFIG_HOME/booru/config.toml`
A very minimal configuration will look like the following
```toml
[[sources]]
provider = "danbooru"
ids = ["5614777", "9590836", "9561442"]
```

an exhaustive list of options is provided by default with booru-hs 
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

> TODO <br>
> for the time being you may take a look at [examples/](examples/)

# Acknowledgements

[toml-parser]

Excellent toml parsing library and served as the foundation for the structure and
tooling for this project.

[booru-flake]

The predecessor to this project, booru-hs is simply an upgrade to booru-flake on most most levels, 
I suppose booru-hs is missing a nixosModule (not for long).

[optparse-applicative]

Excellent CLI framework,
besides a few nitpicks its does it job well and is easy to work with.

[You | the users][stargazers]

Thank you.

# Licensing

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
