{-# LANGUAGE Haskell2010#-}
module Data.Char.Romaja
    ( isKoreanChar
    , isLatinChar
    , romajanize
    , romajanizeChar
    , decomposeKoreanSyllableChar
    , romajanizeConsonant
    , ConsonantPos(..)
    ) where

import           Data.Char
import           Data.Maybe          (maybeToList)
import qualified Data.Text  as T
import           Data.Text.Normalize (normalize, NormalizationMode(NFC))

--import Debug.Trace

traceIt :: Show a => [Char] -> a -> a
traceIt msg val = trace (msg <> " " <> show val) val 
  where
    trace _ a = a

-- * Character classes

-- | Modern Korean characters:
isModernKoreanCharacter :: Char -> Bool
isModernKoreanCharacter               c =
                 isHangulJamo         c -- Hangul Jamo
              || isKoreanSyllableChar c -- Hangul syllables

-- | Korean characters
isKoreanChar :: Char -> Bool
isKoreanChar c = isModernKoreanCharacter c
              -- Archaic forms, Unicode 1.0 and 1.1
              || (ord c >= 0x3130 && ord c <= 0x318F) -- Hangul extended Jamo
              || (ord c >= 0xA960 && ord c <= 0xA97F) -- Hangul extended Jamo A
              || (ord c >= 0xD7B0 && ord c <= 0xD7FF) -- Hangul extended Jamo B
              -- Enclosed or circled
              || (ord c >= 0x3200 && ord c <= 0x321E) -- Enclosed CJK letters and months 1 (parenthesised)
              || (ord c >= 0x3260 && ord c <= 0x327F) -- Enclosed CJK letters and months 2 (circled)
              -- Half-width
              || (ord c >= 0xFFA0 && ord c <= 0xFFDC) -- Half-width Hangul compatibility

-- | Is it standard Hangul Jamo?
isHangulJamo :: Char -> Bool
isHangulJamo c = ord c >= 0x1100 && ord c <= 0x11FF

-- | Is it precomposed Hangul syllable?
isKoreanSyllableChar  :: Char -> Bool
isKoreanSyllableChar c = ord c >= 0xAC00 && ord c <= 0xD7A3 -- Hangul syllables

-- | Is it Korean syllable character 
koreanSyllableCharacter :: Char -> Bool
koreanSyllableCharacter c = ord c >= 44032
                         && ord c <= 44032 + 588*18 + 28*20+27

-- * Syllable decomposition

-- | Convert a Korean syllable char into a list of vowel and consonant Jamos
--
--   NOTE: Alternative way to implement is to use Unicode canonical decomposition,
--         but the we also need to handle upper and lower jamos.
decomposedKoreanSyllable  :: Char -> [Char]
decomposedKoreanSyllable c = [initialChar, medialChar] <> maybeToList finalChar
  where
    (initialChar, medialChar, finalChar) = decomposeKoreanSyllableChar c

decomposeKoreanSyllableChar :: Char -> (Char, Char, Maybe Char)
decomposeKoreanSyllableChar c = (initialChar, medialChar, finalChar)
  where
    (rest   , final ) = (ord c - 44032) `quotRem`            28
    (initial, medial) = rest            `quotRem` (588 `div` 28)
    initialChar = "ㄱㄲㄴㄷㄸㄹㅁㅂㅃㅅㅆㅇㅈㅉㅊㅋㅌㅍㅎ"    !! traceIt "initial" initial
    medialChar  = "ㅏㅐㅑㅒㅓㅔㅕㅖㅗㅘㅙㅚㅛㅜㅝㅞㅟㅠㅡㅢㅣ" !! traceIt "medial"  medial
    finalChar   = case traceIt "final" final of
                    0          -> Nothing
                    otherFinal -> Just $ " ㄱㄲㄳㄴㄵㄶㄷㄹㄺㄻㄼㄽㄾㄿㅀㅁㅂㅄㅅㅆㅇㅈㅊㅋㅌㅍㅎ" !! final

-- | Take a Korean syllable Jamo and romanize it.
--   Non-Korean characters should pass through.
romajanizeKoreanSyllable :: Char -> String
romajanizeKoreanSyllable c | koreanSyllableCharacter c =
    mconcat 
      [romajanizeConsonant Initial initial
      ,romajanizeVowel             medial
      ,maybe ""
      (romajanizeConsonant Final)  final ]
  where
    (initial, medial, final) = decomposeKoreanSyllableChar c
romajanizeKoreanSyllable c = [c] -- pass through

-- | Romanize Korean vowel character.
romajanizeVowel :: Char -> String
romajanizeVowel 'ㅏ' = "a"
romajanizeVowel 'ㅓ' = "eo"
romajanizeVowel 'ㅗ' = "o"
romajanizeVowel 'ㅜ' = "u"
romajanizeVowel 'ㅡ' = "eu"
romajanizeVowel 'ㅣ' = "i"
romajanizeVowel 'ㅐ' = "ae"
romajanizeVowel 'ㅔ' = "e"
romajanizeVowel 'ㅑ' = "ya"
romajanizeVowel 'ㅕ' = "yeo"
romajanizeVowel 'ㅛ' = "yo"
romajanizeVowel 'ㅠ' = "yu"
romajanizeVowel 'ㅒ' = "yae"
romajanizeVowel 'ㅖ' = "ye"
romajanizeVowel 'ㅘ' = "wa"
romajanizeVowel 'ㅚ' = "oe"
romajanizeVowel 'ㅙ' = "wae"
romajanizeVowel 'ㅝ' = "wo"
romajanizeVowel 'ㅟ' = "wi"
romajanizeVowel 'ㅞ' = "we"
romajanizeVowel 'ㅢ' = "ui"
romajanizeVowel  c   = [c]


-- | Position of the Korean consonant withing syllable character.
data ConsonantPos = Initial | Final

-- | Take a consonant and position, and return a romanization of it.
--   Other characters are let pass through.
--
--   TODO: Ask Korean speaker about romanization of: ㄺㄻㄼㄽㄾㄿㅀ. Same for ㄳㄵㄶㅄ
--         Should one first decompose these into ㄹ and another final consonant?
--         Should one apply phonological rules to these
romajanizeConsonant :: ConsonantPos -> Char -> String
romajanizeConsonant Initial 'ㄱ' = "g" 
romajanizeConsonant Final   'ㄱ' = "k"
romajanizeConsonant Initial 'ㄲ' = "kk"
romajanizeConsonant Final   'ㄲ' = "k"
romajanizeConsonant _       'ㄴ' = "n"
romajanizeConsonant Initial 'ㄷ' = "d"
romajanizeConsonant Final   'ㄷ' = "t"
romajanizeConsonant Initial 'ㄸ' = "tt"
romajanizeConsonant Final   'ㄸ' = "" -- TODO: or no character?
romajanizeConsonant Initial 'ㄹ' = "r"
romajanizeConsonant Final   'ㄹ' = "l"
romajanizeConsonant _       'ㅁ' = "m"
romajanizeConsonant Initial 'ㅂ' = "b"
romajanizeConsonant Final   'ㅂ' = "p"
romajanizeConsonant Initial 'ㅃ' = "pp"
romajanizeConsonant Final   'ㅃ' = ""
romajanizeConsonant Initial 'ㅅ' = "s"
romajanizeConsonant Final   'ㅅ' = "t"
romajanizeConsonant Initial 'ㅆ' = "ss"
romajanizeConsonant Final   'ㅆ' = "t"
romajanizeConsonant Initial 'ㅇ' = ""
romajanizeConsonant Final   'ㅇ' = "ng"
romajanizeConsonant Initial 'ㅈ' = "j"
romajanizeConsonant Final   'ㅈ' = "t"
romajanizeConsonant Initial 'ㅉ' = "jj"
romajanizeConsonant Final   'ㅉ' = ""
romajanizeConsonant Initial 'ㅊ' = "ch"
romajanizeConsonant Final   'ㅊ' = "t"
romajanizeConsonant _       'ㅋ' = "k"
romajanizeConsonant _       'ㅌ' = "t"
romajanizeConsonant _       'ㅍ' = "p"
romajanizeConsonant Initial 'ㅎ' = "h"
romajanizeConsonant Final   'ㅎ' = "t"
romajanizeConsonant _        c  = [c]

-- | Romajanize modern composable Jamos
--
--   Modern Jamo character set distinguishes between initial
--   and final vowels. A knowledgeable Korean implementor
--   could use it to make romaja conversion without using
--   @ConsonantPos datatype, but I am too scared that I get it wrong.
romajanizeJamo :: Char -> String
-- Initial consonant Jamos
romajanizeJamo 'ᄀ' = romajanizeConsonant Initial 'ㄱ'
romajanizeJamo 'ᄁ' = romajanizeConsonant Initial 'ㄲ'
romajanizeJamo 'ᄂ' = romajanizeConsonant Initial 'ㄴ'
romajanizeJamo 'ᄃ' = romajanizeConsonant Initial 'ㄷ'
romajanizeJamo 'ᄄ' = romajanizeConsonant Initial 'ㄸ'
romajanizeJamo 'ᄅ' = romajanizeConsonant Initial 'ㄹ'
romajanizeJamo 'ᄆ' = romajanizeConsonant Initial 'ㅁ'
romajanizeJamo 'ᄇ' = romajanizeConsonant Initial 'ㅂ'
romajanizeJamo 'ᄈ' = romajanizeConsonant Initial 'ㅃ'
romajanizeJamo 'ᄉ' = romajanizeConsonant Initial 'ㅅ'
romajanizeJamo 'ᄊ' = romajanizeConsonant Initial 'ㅆ'
romajanizeJamo 'ᄋ' = romajanizeConsonant Initial 'ㅇ'
romajanizeJamo 'ᄌ' = romajanizeConsonant Initial 'ㅈ'
romajanizeJamo 'ᄍ' = romajanizeConsonant Initial 'ㅉ'
romajanizeJamo 'ᄎ' = romajanizeConsonant Initial 'ㅊ'
romajanizeJamo 'ᄏ' = romajanizeConsonant Initial 'ㅋ'
romajanizeJamo 'ᄐ' = romajanizeConsonant Initial 'ㅌ'
romajanizeJamo 'ᄑ' = romajanizeConsonant Initial 'ㅍ'
romajanizeJamo 'ᄒ' = romajanizeConsonant Initial 'ㅎ'
-- Final consonant Jamos
romajanizeJamo 'ᆨ' = romajanizeConsonant Final 'ㄱ'
romajanizeJamo 'ᆩ' = romajanizeConsonant Final 'ㄲ'
romajanizeJamo 'ᆪ' = romajanizeConsonant Final 'ㄱ'
romajanizeJamo 'ᆫ' = romajanizeConsonant Final 'ㄴ'
romajanizeJamo 'ᆭ' = romajanizeConsonant Final 'ㄴ'
romajanizeJamo 'ᆮ' = romajanizeConsonant Final 'ㄷ'
romajanizeJamo 'ᆯ' = romajanizeConsonant Final 'ㄹ'
romajanizeJamo 'ᆰ' = romajanizeConsonant Final 'ㄱ'
romajanizeJamo 'ᆱ' = romajanizeConsonant Final 'ㅁ'
romajanizeJamo 'ᆲ' = romajanizeConsonant Final 'ㄹ'
romajanizeJamo 'ᆳ' = romajanizeConsonant Final 'ㄹ'
romajanizeJamo 'ᆴ' = romajanizeConsonant Final 'ㄹ'
romajanizeJamo 'ᆵ' = romajanizeConsonant Final 'ㅍ'
romajanizeJamo 'ᆶ' = romajanizeConsonant Final 'ㄹ'
romajanizeJamo 'ᆹ' = romajanizeConsonant Final 'ㅂ'
romajanizeJamo 'ᆸ' = romajanizeConsonant Final 'ㅂ'
romajanizeJamo 'ᆺ' = romajanizeConsonant Final 'ㅅ'
romajanizeJamo 'ᆷ' = romajanizeConsonant Final 'ㅁ'
romajanizeJamo 'ᆽ' = romajanizeConsonant Final 'ㅈ'
romajanizeJamo 'ᆾ' = romajanizeConsonant Final 'ㅊ'
romajanizeJamo 'ᆿ' = romajanizeConsonant Final 'ㅋ'
romajanizeJamo 'ᇀ' = romajanizeConsonant Final 'ㅌ'
romajanizeJamo 'ᇁ' = romajanizeConsonant Final 'ㅍ'
romajanizeJamo 'ᇂ' = romajanizeConsonant Final 'ㅎ'
-- Medial vowel Jamos
romajanizeJamo 'ᅡ' = romajanizeVowel 'ㅏ' 
romajanizeJamo 'ᅥ' = romajanizeVowel 'ㅓ' 
romajanizeJamo 'ᅩ' = romajanizeVowel 'ㅗ' 
romajanizeJamo 'ᅮ' = romajanizeVowel 'ㅜ' 
romajanizeJamo 'ᅳ' = romajanizeVowel 'ㅡ' 
romajanizeJamo 'ᅵ' = romajanizeVowel 'ㅣ' 
romajanizeJamo 'ᅢ' = romajanizeVowel 'ㅐ' 
romajanizeJamo 'ᅦ' = romajanizeVowel 'ㅔ' 
romajanizeJamo 'ᅣ' = romajanizeVowel 'ㅑ' 
romajanizeJamo 'ᅧ' = romajanizeVowel 'ㅕ' 
romajanizeJamo 'ᅭ' = romajanizeVowel 'ㅛ' 
romajanizeJamo 'ᅲ' = romajanizeVowel 'ㅠ' 
romajanizeJamo 'ᅤ' = romajanizeVowel 'ㅒ' 
romajanizeJamo 'ᅨ' = romajanizeVowel 'ㅖ' 
romajanizeJamo 'ᅪ' = romajanizeVowel 'ㅘ' 
romajanizeJamo 'ᅬ' = romajanizeVowel 'ㅚ' 
romajanizeJamo 'ᅫ' = romajanizeVowel 'ㅙ' 
romajanizeJamo 'ᅯ' = romajanizeVowel 'ㅝ' 
romajanizeJamo 'ᅱ' = romajanizeVowel 'ㅟ' 
romajanizeJamo 'ᅰ' = romajanizeVowel 'ㅞ' 
romajanizeJamo 'ᅴ' = romajanizeVowel 'ㅢ' 
-- pass through for ancient Jamos
romajanizeJamo other = [other] -- Not a Jamo, or from an unsupported block

-- | Check if character is from latin character subset.
isLatinChar :: Char -> Bool
isLatinChar = isLatin1

-- | Look at a pair of final consonant and the following initial consonant.
--
--   Implement phonological change for the pair if necessary.
--   Pass both consonants to the usual romanization algorithm otherwise. 
--
--  NOTES:
--
--    * alternative way to implement phonological changes
--      would be to use composable jamos instead,
--      and use Unicode decomposition before processing them.
--      This would be tricky to debug with non-Korean fonts.
--    * some phonological changes are ambiguous
--
phonology :: Char -- final consonant of previous syllable
          -> Char -- initial consonant of the following syllable
          -> String -- romanized _pair_
phonology 'ㄱ' 'ㅇ' = "g"
phonology 'ㄱ' 'ㄴ' = "ngn"
phonology 'ㄱ' 'ㄹ' = "ngn"
phonology 'ㄱ' 'ㅁ' = "ngm"
phonology 'ㄱ' 'ㅎ' = "k" -- instead of "kh"
phonology 'ㄴ' 'ㄹ' = "nn"
phonology 'ㄷ' 'ㅇ' = "d" -- or "j"
phonology 'ㄷ' 'ㄴ' = "nn"
phonology 'ㄷ' 'ㄹ' = "nn"
phonology 'ㄷ' 'ㅁ' = "nm"
phonology 'ㄷ' 'ㅎ' = "th" -- or "t", "ch"
phonology 'ㄹ' 'ㅇ' = "r"
phonology 'ㅁ' 'ㄹ' = "mn"
phonology 'ㅂ' 'ㅇ' = "b"
phonology 'ㅂ' 'ㄴ' = "mn"
phonology 'ㅂ' 'ㄹ' = "mn"
phonology 'ㅂ' 'ㅁ' = "mm"
phonology 'ㅂ' 'ㅎ' = "ph" -- "p"
phonology 'ㅅ' 'ㅇ' = "s"
phonology 'ㅅ' 'ㄴ' = "nn"
phonology 'ㅅ' 'ㄹ' = "nn"
phonology 'ㅅ' 'ㅁ' = "nm"
phonology 'ㅇ' 'ㄹ' = "ngn"
phonology 'ㅈ' 'ㅇ' = "j"
phonology 'ㅈ' 'ㄴ' = "nn"
phonology 'ㅈ' 'ㄹ' = "nn"
phonology 'ㅈ' 'ㅁ' = "nm"
phonology 'ㅈ' 'ㅎ' = "th" -- "t", "ch"
phonology 'ㅊ' 'ㅇ' = "ch"
phonology 'ㅊ' 'ㄴ' = "nn"
phonology 'ㅊ' 'ㄹ' = "nn"
phonology 'ㅊ' 'ㅁ' = "nm"
phonology 'ㅊ' 'ㅎ' = "th" -- "t", "ch"
-- A bit off the beaten track
phonology 'ㅎ' 'ㅇ' = "h"
phonology 'ㅎ' 'ㄱ' = "k"
phonology 'ㅎ' 'ㄴ' = "nn"
phonology 'ㅎ' 'ㄷ' = "t"
phonology 'ㅎ' 'ㄹ' = "nn"
phonology 'ㅎ' 'ㅁ' = "nm"
phonology 'ㅎ' 'ㅂ' = "p"
phonology 'ㅎ' 'ㅈ' = "ch"
phonology 'ㅎ' 'ㅌ' = "t"
phonology  p   n   = romajanizeConsonant Final   p
                  <> romajanizeConsonant Initial n

-- | Romanize Korean character.
--
--   Currently only syllabic characters are handled.
romajanizeChar :: Char -> String
romajanizeChar c | isKoreanSyllableChar c = romajanizeKoreanSyllable c
romajanizeChar c | isHangulJamo         c = romajanizeJamo           c
romajanizeChar c                          = case romajanizeVowel c of
                                              [c]   -> romajanizeConsonant Initial c
                                              other -> other
romajanizeChar c                          =                         [c] -- pass through

-- | Romajanize Korean characters in the String.
--
--   Other characters are passed through.
romajanize :: String -> String 
romajanize = mconcat . map romajanizeChar
           . T.unpack . normalize NFC . T.pack -- canonicalize Unicode
