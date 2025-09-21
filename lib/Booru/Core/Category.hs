module Booru.Core.Category (
  Category (..),
  TagMap,
  newCategory,
  genCategory,
  getImageCat,
) where

import Booru.Schema.Images (Image (..), Tag, resolvedName)
import Data.Map (Map, empty, insert, unionWith)
import Data.Set (Set, singleton, union)

data Category = Category
  { artistC :: TagMap
  , copyrightC :: TagMap
  , characterC :: TagMap
  }
  deriving (Show, Eq)
type TagMap = Map Tag (Set String)

instance Semigroup Category where
  (<>) cat1 cat2 =
    cat1
      { artistC = aux (artistC cat1) (artistC cat2)
      , copyrightC = aux (copyrightC cat1) (copyrightC cat2)
      , characterC = aux (characterC cat1) (characterC cat2)
      }
   where
    aux = unionWith union

newCategory :: Category
newCategory =
  Category
    { artistC = empty
    , copyrightC = empty
    , characterC = empty
    }

genCategory :: [Image] -> Category
genCategory = foldr ((<>) . getImageCat) newCategory

getImageCat :: Image -> Category
getImageCat img =
  Category
    { artistC = ctm (artists img)
    , copyrightC = ctm (copyrights img)
    , characterC = ctm (characters img)
    }
 where
  ctm = createTagMap img

createTagMap :: Image -> [Tag] -> TagMap
createTagMap img [] = createTagMap img ["unknown"]
createTagMap Image{resolvedName = rname} ts = foldr (`insert` singleton rname) empty ts
