module Main where

import Test.Hspec
import Data.Char(toLower)
import Data.Char.Romaji
import Data.Text.Normalize
import qualified Data.Text as T

checkRomajization jp en =
  it (jp <> " is romanized to " <> en) $
    romajize jp `shouldBe` T.unpack (normalize NFD $ T.pack $ fmap toLower en)

main :: IO ()
main = hspec $ do
  describe "Wikipedia examples" $ do
    describe "hiragana" $ do 
        checkRomajization "ローマじ" "rômazi"
        checkRomajization "ふじさん" "Huzisan"
        checkRomajization "おちゃ"   "otya"
        checkRomajization "ちじ"     "tizi"
        checkRomajization "ちぢむ"   "tizimu"
        checkRomajization "つづく"   "tuzuku"
        checkRomajization "非ヘボン式ローマ字"   "hi-Hebon-shiki rōmaji"
    describe "Mixed Hiragana and Kanji" $ do 
        checkRomajization "結婚する"  "kekkonsuru"