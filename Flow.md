# Flow of control
> This file is preserved for historical reasons.
> This is simply a train of thought dumped into an md file.
> Updated and referenced over the course of building booru-hs.

```
parse configuration
| infer providers
| infer synonyms
| infer sources

identify missing files
| reduces the parsed information to simply include only files
| that are not presently in the database. NOTE 1

  get json Info for missing files
  | essentially we retreive information about the file to download
  | we would store this info in the DB if we plan on using sqlite or similar
  | for 'urls' provider and the like this step is skipped

  create image strucutre containing all information
  | Image {
  |   dbname "<qualified representation>",
  |   artists,
  |   characters,
  |   copyrights,
  |   tags,
  |   rating,
  | }
  | information here is based off of provider spec

  pass image structure through Overrides

  pass image structure throught Synonyms

  store image structure information into db/info/<qualified name>.toml
  | again, this step may not be required if I decide to use a proper
  | db to inser this information into

  download images into central folder
  | with the current proposal the db is a folder
  | where all images are dumped

| At this point, we should have a list containing information about available and missing files combined

collect previews using preview filter -> PREVIEW_IMAGES

fold information into structured set
| the bread and butter of booru flake
| at the end of this step we should have structure like so
| (<attribute>, (<attribute_value>, [Image]))
| where attribute is character, copyright, artist, (optional) provider, (optional) tag

filter the categorized structure use FILTERS
| this filter, unlike preview filters is used to filter out
| creation of uneeded folders more info in filter spec

leverage the above structure to create symlinks at DEST directory
| read the file system at DEST into a Category
| let genCat = new category
| let fsCat = category in file system
| then, files to create =  genCat - fsCat
| and, files to delete = fsCat - genCat
```

#### NOTE 1
the database I am thinking of at the moment is simply a folder containing
`provider`-(hash of `id`).

####  NOTE 2
add a gc step to remove unused files and data from the db folder

#### Note 3
add option to force update information for a specified source
uhh ideally you don't wanna do this, specially if you've amassed a large collection
but yeah this must be provided as an option
