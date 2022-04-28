module Main where

import Test.Hspec
import Data.Char(toLower)
import Data.Char.Romaji
import Data.Text.Normalize
import qualified Data.Text as T

checkRomajization jp en =
  it (jp <> " is romanized to " <> en) $
    romajize jp `shouldBe` T.unpack (normalize NFD $ T.pack $ fmap toLower en)

-- | For now translation and romajization are the same.
checkTranslation jp en = checkRomajization jp en

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
        checkRomajization "あっ"   "at"
    describe "Mixed Hiragana and Kanji" $ do 
        checkRomajization "結婚する"  "kekkonsuru"
  describe "monash" $ do -- https://guides.lib.monash.edu/c.php?g=922671&p=6721790
    checkRomajization "お婆さん"   "obāsan"
    checkRomajization "おばあさん" "obāsan"
    checkRomajization "勉強"      "benkyō"
    checkRomajization "べんきょう" "benkyō"
    checkRomajization "大阪"      "Ōsaka"
    checkRomajization "おおさか"   "Ōsaka"
  describe "loc.gov" $ do -- https://www.loc.gov/catdir/cpso/romanization/japanese.pdf -- is it Hepburn or Kureinshiki?
    checkRomajization "横浜"      "Yokohama"
    checkRomajization "アメリカ人" "Amerikajin"
    checkRomajization "日本語"     "Nihongo"
  describe "hiragana writing practice" $ do -- https://guidetojapanese.org/learn/grammar/hiragana_ex
    checkRomajization "じゅぎょう" "jyugyo"
    checkRomajization "たべもの"   "tabemono"
    checkRomajization "とった"     "totta"
  describe "stumbling words" $ do  -- https://nihongodera.com/tools/convert
    checkRomajization "えっと"     "e to"
    checkRomajization "あの"       "ano"
  describe "translation" $ do
    checkTranslation  "プルデンシャルジブラルタファイナンシャル生命保険株式会社" "Prudential Gibralta Financial Life Insurance Co., Ltd."
