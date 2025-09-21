{-# LANGUAGE DisambiguateRecordFields #-}

module Core.PreviewSpec (spec) where

import Booru.Core.Preview
import Booru.Schema.Filters (Filter (..))
import Booru.Schema.Identifier
import Booru.Schema.Images (Image (..))
import Booru.Schema.PFilters (PFilters (..))
import qualified Booru.Schema.PFilters as PF
import Test.Hspec (Spec, it, shouldBe)

imgWPrv :: Image
imgWPrv =
  Image
    { resolvedName = "generalImg"
    , provider = "danbooru"
    , id = WithNick{id = "9969513", nickname = "kokomi_chibbi"}
    , file = "https://safebooru.org/images/539/6fef5929e6e09703b1493c6e5802fa5e4f189639.jpg"
    , preview_file = "https://safebooru.org/samples/539/sample_6fef5929e6e09703b1493c6e5802fa5e4f189639.jpg"
    , artists = []
    , characters = []
    , copyrights = []
    , rating = "g"
    , tags = ["cat"]
    }

imgWoPrv :: Image
imgWoPrv = imgWPrv{preview_file = ""}

generatedPreview :: [String]
generatedPreview =
  [ "| Column 1 | Column 2 | Column 3 | Column 4 |"
  , "|----------|----------|----------|----------|"
  , "|[![generalImg](https://safebooru.org/samples/539/sample_6fef5929e6e09703b1493c6e5802fa5e4f189639.jpg)](https://safebooru.org/images/539/6fef5929e6e09703b1493c6e5802fa5e4f189639.jpg)|![generalImg](https://safebooru.org/images/539/6fef5929e6e09703b1493c6e5802fa5e4f189639.jpg)|![generalImg](https://safebooru.org/images/539/6fef5929e6e09703b1493c6e5802fa5e4f189639.jpg)|![generalImg](https://safebooru.org/images/539/6fef5929e6e09703b1493c6e5802fa5e4f189639.jpg)|"
  , "|![generalImg](https://safebooru.org/images/539/6fef5929e6e09703b1493c6e5802fa5e4f189639.jpg)|"
  ]

prvFilter :: PFilters
prvFilter =
  PFilters
    { characters = Just Filter{list = ["kokomi"], inverted = False}
    , copyrights = Just Filter{list = ["genshin_impact"], inverted = False}
    , artists = Just Filter{list = ["mourncolor"], inverted = False}
    , tags = Just Filter{list = ["s34"], inverted = False}
    , ids = Just Filter{list = ["BKA"], inverted = False}
    , ratings = Just Filter{list = ["g"], inverted = False}
    , providers = Just Filter{list = ["danbooru"], inverted = False}
    }

invertFilter :: Filter -> Maybe Filter
invertFilter ft@Filter{inverted = inv} = Just ft{inverted = not inv}

prvFilter' :: PFilters
prvFilter' =
  PFilters
    { characters = PF.characters prvFilter >>= invertFilter
    , copyrights = PF.copyrights prvFilter >>= invertFilter
    , artists = PF.artists prvFilter >>= invertFilter
    , tags = PF.tags prvFilter >>= invertFilter
    , ids = ids prvFilter >>= invertFilter
    , ratings = ratings prvFilter >>= invertFilter
    , providers = providers prvFilter >>= invertFilter
    }

emptyFilter :: PFilters
emptyFilter =
  PFilters
    { characters = Nothing
    , copyrights = Nothing
    , artists = Nothing
    , tags = Nothing
    , ids = Nothing
    , ratings = Nothing
    , providers = Nothing
    }

bakaImg :: Image
bakaImg =
  Image
    { resolvedName = "baka"
    , provider = "danbooru"
    , id = Id "BKA"
    , file = "ignored"
    , preview_file = "ignored"
    , artists = ["mourncolor"]
    , characters = ["kokomi"]
    , copyrights = ["genshin_impact"]
    , rating = "g"
    , tags = ["s34"]
    }

bakaImg' :: Image
bakaImg' =
  Image
    { resolvedName = "notbaka"
    , provider = "notdanbooru"
    , id = Id "NBKA"
    , file = "ignored"
    , preview_file = "ignored"
    , artists = ["colormourn"]
    , characters = ["kokolala"]
    , copyrights = ["honkaiSteelBallRun"]
    , rating = "e"
    , tags = ["r84"]
    }

mapToName :: [Image] -> [String]
mapToName = map resolvedName

spec :: Spec
spec = do
  it "generates preview.md from image list" $ lines (generatePreview [imgWPrv, imgWoPrv, imgWoPrv, imgWoPrv, imgWoPrv]) `shouldBe` generatedPreview
  it "filters images" $ mapToName (filterImages (Just prvFilter) [bakaImg, bakaImg']) `shouldBe` [resolvedName bakaImg']
  it "filters images # inverted" $ mapToName (filterImages (Just prvFilter') [bakaImg, bakaImg']) `shouldBe` [resolvedName bakaImg]
  it "filters images # empty" $ mapToName (filterImages (Just emptyFilter) [bakaImg, bakaImg']) `shouldBe` mapToName [bakaImg, bakaImg']
