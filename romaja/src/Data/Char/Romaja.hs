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

import Data.Char
import Data.Maybe(maybeToList)

import Debug.Trace

traceIt :: Show a => [Char] -> a -> a
traceIt msg val = trace (msg <> " " <> show val) val 

isKoreanChar :: Char -> Bool
isKoreanChar c = isHangulJamo c -- Hangul Jamo
              || (ord c >= 0x3130 && ord c <= 0x318F) -- Hangul extended Jamo
              || (ord c >= 0xA960 && ord c <= 0xA97F) -- Hangul extended Jamo A
              || (ord c >= 0xD7B0 && ord c <= 0xD7FF) -- Hangul extended Jamo B
              || (ord c >= 0x3200 && ord c <= 0x321E) -- Enclosed CJK letters and months 1 (parenthesised)
              || (ord c >= 0x3260 && ord c <= 0x327F) -- Enclosed CJK letters and months 2 (circled)
              || (ord c >= 0xFFA0 && ord c <= 0xFFDC) -- Half-width Hangul compatibility
              || isKoreanSyllableChar c -- Hangul syllables

-- | Is it standard Hangul Jamo?
isHangulJamo :: Char -> Bool
isHangulJamo c = ord c >= 0x1100 && ord c <= 0x11FF

-- | Is it precomposed Hangul syllable?
isKoreanSyllableChar  :: Char -> Bool
isKoreanSyllableChar c = ord c >= 0xAC00 && ord c <= 0xD7A3 -- Hangul syllables

-- | Convert a Korean syllable char into a list of vowel and consonant Jamos
decomposedKoreanSyllable :: Char -> [Char]
decomposedKoreanSyllable c = [initialChar, medialChar] <> maybeToList finalChar
  where
    (initialChar, medialChar, finalChar) = decomposeKoreanSyllableChar c
 
koreanSyllableCharacter c = ord c >= 44032
                         && ord c <= 44032 + 588*18 + 28*20+27
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


-- | Position of the Korean consonant withing syllable character.
data ConsonantPos = Initial | Final

-- | Take a consonant and position, and return a romanization of it.
--   Other characters are let pass through.
romajanizeConsonant :: ConsonantPos -> Char -> String
romajanizeConsonant Initial 'ㄱ' = "g" 
romajanizeConsonant Final   'ㄱ' = "k"
romajanizeConsonant Initial 'ㄲ' = "kk"
romajanizeConsonant Final   'ㄲ' = "k"
romajanizeConsonant _       'ㄴ' = "k"
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
romajanizeJamo 'ᄈ' = romajanizeConsonant Initial 'ㅂ'
romajanizeJamo 'ᄉ' = romajanizeConsonant Initial 'ㅃ'
romajanizeJamo 'ᄊ' = romajanizeConsonant Initial 'ㅅ'
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
romajanizeJamo 'ᆪ' = romajanizeConsonant Final 'ㄱ' -- TODO: Ask So about guess on compositions
                   <> romajanizeConsonant Final 'ㅅ'
romajanizeJamo 'ᆫ' = romajanizeConsonant Final 'ㄴ'
romajanizeJamo 'ᆭ' = romajanizeConsonant Final 'ㄴ'
                   <> romajanizeConsonant Final 'ㅎ'
romajanizeJamo 'ᆮ' = romajanizeConsonant Final 'ㄷ'
romajanizeJamo 'ᆯ' = romajanizeConsonant Final 'ㄹ'
romajanizeJamo 'ᆰ' = romajanizeConsonant Final 'ㄹ'
                   <> romajanizeConsonant Final 'ㄱ'
romajanizeJamo 'ᆱ' = romajanizeConsonant Final 'ㄹ'
                   <> romajanizeConsonant Final 'ㅇ'
romajanizeJamo 'ᆲ' = romajanizeConsonant Final 'ㄹ'
                   <> romajanizeConsonant Final 'ㅂ'
romajanizeJamo 'ᆳ' = romajanizeConsonant Final 'ㄹ'
                   <> romajanizeConsonant Final 'ㅅ'
romajanizeJamo 'ᆴ' = romajanizeConsonant Final 'ㄹ'
                   <> romajanizeConsonant Final 'ㅌ'
romajanizeJamo 'ᆵ' = romajanizeConsonant Final 'ㄹ'
                   <> romajanizeConsonant Final 'ㅍ'
romajanizeJamo 'ᆶ' = romajanizeConsonant Final 'ㄹ'
                   <> romajanizeConsonant Final 'ㅎ'
romajanizeJamo 'ᆸ' = romajanizeConsonant Final 'ㅂ'
romajanizeJamo 'ᆺ' = romajanizeConsonant Final 'ㅅ'
romajanizeJamo 'ᆷ' = romajanizeConsonant Final 'ㅇ'
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
romajanizeJamo other = [other] -- Not a Jamo, or from compatibility block

-- | Check if character is from latin character subset.
isLatinChar :: Char -> Bool
isLatinChar = isLatin1

-- | Romanize Korean character.
--
--   Currently only syllabic characters are handled.
romajanizeChar :: Char -> String
romajanizeChar c | isKoreanSyllableChar c = romajanizeKoreanSyllable c
romajanizeChar c | isHangulJamo         c = romajanizeJamo           c
romajanizeChar c                          =                         [c] -- pass through

-- | Romajanize Korean characters in the String.
--
--   Other characters are passed through.
romajanize :: String -> String 
romajanize = mconcat . map romajanizeChar
