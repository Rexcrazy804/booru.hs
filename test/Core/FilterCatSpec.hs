module Core.FilterCatSpec (spec) where

import Booru.Core.Category (Category (..))
import Booru.Core.FilterCat
import Booru.Schema.Filters (Filter (..), Filters (..))
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Hspec (Spec, it, shouldBe)

multiImgCat :: Category
multiImgCat =
  Category
    { artistC =
        M.fromList
          [ ("mikurumikurumi", S.fromList ["danbooru|-8916590893211164014"])
          , ("togo_uzuki", S.fromList ["danbooru|-4278074931642418858"])
          ]
    , copyrightC =
        M.fromList
          [ ("genshin_impact", S.fromList ["danbooru|-4278074931642418858", "danbooru|-8916590893211164014"])
          , ("zenless_zone_zero", S.fromList ["danbooru|-4278074931642418858"])
          ]
    , characterC =
        M.fromList
          [ ("nilou_(genshin_impact)", S.fromList ["danbooru|-4278074931642418858"])
          , ("sangonomiya_kokomi", S.fromList ["danbooru|-8916590893211164014"])
          ]
    }

multiImgCat' :: Category
multiImgCat' =
  Category
    { artistC = M.fromList [("mikurumikurumi", S.fromList ["danbooru|-8916590893211164014"])]
    , copyrightC = M.fromList [("genshin_impact", S.fromList ["danbooru|-4278074931642418858", "danbooru|-8916590893211164014"])]
    , characterC = M.fromList [("sangonomiya_kokomi", S.fromList ["danbooru|-8916590893211164014"])]
    }

multiImgCat'' :: Category
multiImgCat'' =
  Category
    { artistC = M.fromList [("togo_uzuki", S.fromList ["danbooru|-4278074931642418858"])]
    , copyrightC = M.fromList [("zenless_zone_zero", S.fromList ["danbooru|-4278074931642418858"])]
    , characterC = M.fromList [("nilou_(genshin_impact)", S.fromList ["danbooru|-4278074931642418858"])]
    }

simpleFilter :: Filters
simpleFilter =
  Filters
    { characters = Just $ Filter{list = ["sangonomiya_kokomi"], inverted = False}
    , copyrights = Just $ Filter{list = ["genshin_impact"], inverted = False}
    , artists = Just $ Filter{list = ["mikurumikurumi"], inverted = False}
    }

invertFilter :: Filter -> Maybe Filter
invertFilter ft@Filter{inverted = inv} = Just ft{inverted = not inv}

simpleFilter' :: Filters
simpleFilter' =
  Filters
    { characters = characters simpleFilter >>= invertFilter
    , copyrights = copyrights simpleFilter >>= invertFilter
    , artists = artists simpleFilter >>= invertFilter
    }

spec :: Spec
spec = do
  it "categorizes simple image" $ filterCategory (Just simpleFilter) multiImgCat `shouldBe` multiImgCat''
  it "categorizes simple image # inverted" $ filterCategory (Just simpleFilter') multiImgCat `shouldBe` multiImgCat'
