{-# LANGUAGE PartialTypeSignatures #-} -- Dev only
{-# LANGUAGE DerivingVia           #-}
module Main where

import Control.Monad(forM_, zipWithM_, when)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Expectations
import Test.QuickCheck
import Test.QuickCheck.Arbitrary(vector)
import Data.Char.Romaja
import Data.Char

newtype ModernKoreanString = ModernKoreanString { unModernKoreanString :: String }
  deriving (Show,                Eq) via String

-- | The commonly used Modern Korean characters:
-- Standard Jamo range:     0x1100 0x11FF
-- Standard syllable range: 0xAC00 0xD7A3
--
-- Does not include unused to Unicode from KS X 1002 in Unicode 1.1:
-- U+3D2E to U+44B7
-- U+44B8 to U+44BD
-- U+44BE to U+4DFF
--
-- Does not include Unicode 1.0.0 from KS C 5691-1987
-- U+3400–U+3D2D
-- These were superseded by syllables block
newtype ModernKoreanChar   = ModernKoreanChar   { unModernKoreanChar   :: Char   }
  deriving (Show, Enum, Bounded, Eq) via Char

instance Arbitrary ModernKoreanChar where
  arbitrary = do
    i <- choose (0, (0x11FF-0x1100)+
                    (0xD7A3-0xAC00))
    return $ ModernKoreanChar $ chr $
      if i <= 0xFF
        then 0x1100         + i
        else 0xAC00 - 0x100 + i

instance Arbitrary ModernKoreanString where
  arbitrary = sized $ \l -> do
    s <- choose (0, l)
    ModernKoreanString <$>
      (vectorOf s $
         fmap unModernKoreanChar $ arbitrary)

checkCharacterRomanization :: [Char] -> [String] -> Spec
checkCharacterRomanization = zipWithM_ checkRomajanizeChar

checkRomajanizeChar :: Char -> String -> Spec
checkRomajanizeChar kc lc = it ("Romanizes " <> [kc] <> " as " <> show lc)
                          $ romajanizeChar kc `shouldBe` lc

checkRomajanize :: String -> String -> Spec
checkRomajanize kc lc = it ("Romanizes " <> kc <> " as " <> lc)
                      $ romajanize kc `shouldBe` lc


main :: IO ()
main = hspec $ do
  it "Detects Korean character" $
    isKoreanChar '년' `shouldBe` True
  it "Detects 년 is not latin character" $
    isLatinChar '년' `shouldBe` False
  it "Decomposes Korean syllable character 한" $
    decomposeKoreanSyllableChar '한' `shouldBe` ('ㅎ',  'ㅏ', Just 'ㄴ')
  it "Romajanize initial consonant ㅃ" $
    romajanizeConsonant Initial 'ㅃ' `shouldBe` "pp"
  it "Romajanize final consonant ㅇ" $
    romajanizeConsonant Final 'ㅇ' `shouldBe` "ng"
  describe "Correctly romanizes vowels" $
    checkCharacterRomanization ['ㅏ','ㅐ','ㅑ','ㅒ','ㅓ','ㅔ','ㅕ','ㅖ','ㅗ','ㅘ','ㅙ','ㅚ','ㅛ','ㅜ','ㅝ','ㅞ','ㅟ','ㅠ','ㅡ','ㅢ','ㅣ']
                               ["a", "ae", "ya", "yae", "eo", "e", "yeo", "ye", "o", "wa", "wae", "oe", "yo", "u", "wo", "we", "wi", "yu", "eu", "ui", "i"]
  describe "Correctly romanizes initial consonants" $
    checkCharacterRomanization ['ㄱ', 'ㄲ', 'ㄴ', 'ㄷ', 'ㄸ', 'ㄹ', 'ㅁ', 'ㅂ', 'ㅃ', 'ㅅ', 'ㅆ', 'ㅇ', 'ㅈ', 'ㅉ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ']
                               ["g", "kk", "n", "d", "tt", "r", "m", "b", "pp", "s", "ss", "", "j", "jj", "ch", "k", "t", "p", "h"]
  it "Should romanize a Korean syllable character" $
    romajanize "항" `shouldBe` "hang"
  it "Should correctly romanize a syllable character for '벽': the 'wall'" $
    romajanize "벽" `shouldBe` "byeok"
  it "Should correctly romanize a syllable character for '밖': the 'outside'" $
    romajanize "밖" `shouldBe` "bak"
  it "Should correctly romanize composed Jamos for '밖': the 'outside'" $
    romajanize "밖" `shouldBe` "bak"
  describe "Double consonant examples" $ do 
    checkRomajanize "넋"    "neok"
    checkRomajanize "넋이"  "neoksi"
    checkRomajanize "여덟"  "yeodeol"
    checkRomajanize "넓은"  "neolbeun"
    checkRomajanize "외곬"  "oegol"
    checkRomajanize "외곬이" "oegolsi"
    checkRomajanize "핥다"  "halda"
    checkRomajanize "핥아"  "halta"
    checkRomajanize "값"    "gap"
    checkRomajanize "값이"   "gapsi"
    checkRomajanize "않고"   "anko"
    checkRomajanize "많이"   "manhi"
    checkRomajanize "싫다"   "silta"
    checkRomajanize "싫어"   "silheo"
    checkRomajanize "닭"     "dak"
    checkRomajanize "맑은"    "malgeun"
    checkRomajanize "삶"     "sam"
    checkRomajanize "짊어지다" "jilmeojida"
    checkRomajanize "읊다"    "eupda"
    checkRomajanize "읊어"    "eulpeo"
    checkRomajanize "읊는"    "eumneun"
    checkRomajanize "놓고"    "noko"
    checkRomajanize "많다"    "manta"
    checkRomajanize "쌓지"    "ssachi"
    checkRomajanize "좋은"    "joeun"
    checkRomajanize "낳은"    "naeun"
    checkRomajanize "쌈짓돈"  "ssamjitdon"
    checkRomajanize "백마"    "Baengma"
    checkRomajanize "신문로"   "Sinmunno"
    checkRomajanize "종로"    "Jongno"
    checkRomajanize "왕십리"   "Wangsimni"
    checkRomajanize "별내"    "Byeollae"
    checkRomajanize "신라"    "Silla"
    checkRomajanize "학여울"  "Hangnyeoul"
    checkRomajanize "알약"    "allyak"
    checkRomajanize "해돋이"   "haedoji"
    checkRomajanize "같이"    "gachi"
    checkRomajanize "맞히다"   "machida"
    checkRomajanize "좋고"    "joko"
    checkRomajanize "놓다"    "nota"
    checkRomajanize "잡혀"    "japyeo"
    checkRomajanize "낳지"    "nachi"
  it "Correctly romanizes a name 정석민" $
    romajanize "정 석민" `shouldBe` (toLower <$> "Jeong Seokmin")
  it "Correctly romanizes a name  최빛나" $
    romajanize "최빛나" `shouldBe` "choebitna"
  it "Correctly romanizes a phonological change of ㄱ, ㄷ, ㅂ and ㅈ are adjacent to ㅎ" $
    romajanize "좋고" `shouldBe` "joko"
  it "Correctly romanizes aspirated sound when ㅎ follows ㄱ, ㄷ and ㅂ" $
    romajanize "묵호" `shouldBe` "mukho"
  it "Should correctly romanize example" $
    romajanize "한국어" `shouldBe` "hangug-eo"
  it "Should correctly romanize a sentence" $
    romajanize "한국은 네 계절이 뚜렷하다." `shouldBe` "Hangugeun ne gyejeori tturyeotada."
  prop "Romanizes any Korean text" $
    \(ModernKoreanString txt) -> forM_ (romajanize txt) (`shouldSatisfy` isLatinChar)


-- Example of Korean from http://columnist.org/parkk/infoage/romaniz.htm
--example = "91년에 프랑스 주간 시사잡지 몇 가지를 구독신청했더니 경쟁적으로 선물을 보내 주었다.그 가운데서 가장 요긴하게 쓴 것은 명함 크기 만한 전자수첩이었다.이 물건은 전화번호 1만 개를 기억시킬 수 있고 수표 발행 기록과 세 가지 신용카드의 사용 기록도 할 수 있어서 편리했다.다만 한국 사람 이름은 모두 로마자로 써서 입력해야 했으므로 내 나름의 표기법을 만들어 썼다.":w
