module Booru.Core (
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
  , copyrightsC :: TagMap
  , charactersC :: TagMap
  }
  deriving (Show, Eq)
type TagMap = Map Tag (Set String)

instance Semigroup Category where
  (<>) cat1 cat2 =
    cat1
      { artistC = aux (artistC cat1) (artistC cat2)
      , copyrightsC = aux (copyrightsC cat1) (copyrightsC cat2)
      , charactersC = aux (charactersC cat1) (charactersC cat2)
      }
   where
    aux = unionWith union

newCategory :: Category
newCategory =
  Category
    { artistC = empty
    , copyrightsC = empty
    , charactersC = empty
    }

genCategory :: [Image] -> Category
genCategory = foldr ((<>) . getImageCat) newCategory

getImageCat :: Image -> Category
getImageCat img =
  Category
    { artistC = ctm (artists img)
    , copyrightsC = ctm (copyrights img)
    , charactersC = ctm (characters img)
    }
 where
  ctm = createTagMap img

createTagMap :: Image -> [Tag] -> TagMap
createTagMap Image{resolvedName = rname} = foldr (`insert` singleton rname) empty
