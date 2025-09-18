module Booru.Core (
  Category (..),
  TagMap,
  newCategory,
  genCategory,
  getImageCat,
) where

import Booru.Schema.Images (Image (..), resolvedName)
import Booru.Schema.Sources (Tag)
import Data.Map (Map, empty)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

data Category = Category
  { artistC :: TagMap
  , copyrightsC :: TagMap
  , charactersC :: TagMap
  }
  deriving (Show, Eq)
type TagMap = Map Tag (Set String)

newCategory :: Category
newCategory =
  Category
    { artistC = empty
    , copyrightsC = empty
    , charactersC = empty
    }

instance Semigroup Category where
  (<>) cat1 cat2 =
    cat1
      { artistC = aux (artistC cat1) (artistC cat2)
      , copyrightsC = aux (copyrightsC cat1) (copyrightsC cat2)
      , charactersC = aux (charactersC cat1) (charactersC cat2)
      }
   where
    aux = M.unionWith S.union

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
createTagMap Image{resolvedName = rname} = foldr (`M.insert` S.singleton rname) empty
