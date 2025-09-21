module Booru.Core.FilterCat (filterCategory) where

import Booru.Core.Category (Category (..), TagMap)
import Booru.Schema.Filters (Filter (..), Filters (..))
import Data.Bits (xor)
import Data.Map (filterWithKey)

filterCategory :: Maybe Filters -> Category -> Category
filterCategory Nothing cat = cat
filterCategory (Just ft) cat =
  Category
    { artistC = filterCatField (artists ft) (artistC cat)
    , copyrightC = filterCatField (copyrights ft) (copyrightC cat)
    , characterC = filterCatField (characters ft) (characterC cat)
    }

filterCatField :: Maybe Filter -> TagMap -> TagMap
filterCatField (Just Filter{list = fts, inverted = inv}) = filterWithKey (\k _ -> not inv `xor` (k `elem` fts))
filterCatField Nothing = id
