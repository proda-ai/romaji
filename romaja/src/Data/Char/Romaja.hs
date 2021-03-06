{-# LANGUAGE Haskell2010#-}
module Data.Char.Romaja
    ( isKoreanChar
    , isLatinChar
    , romajanize
    , romajanizeChar
    , romajanizeKoreanWord
    , decomposeKoreanSyllableChar
    , romajanizeConsonant
    , ConsonantPos(..)
    ) where

import           Control.Exception   (assert)
import           Data.Char
import           Data.Maybe          (maybeToList)
import qualified Data.Text  as T
import           Data.Text.Normalize (normalize, NormalizationMode(..))

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
decomposeKoreanSyllableChar c = assert (           isHangulJamo initialChar)
                              $ assert (           isHangulJamo medialChar )
                              $ assert (maybe True isHangulJamo finalChar  )
                              $ traceIt "decomposition"
                              $ (initialChar, medialChar, finalChar)
  where
    (rest   , final ) = (ord c - 44032) `quotRem`            28
    (initial, medial) = rest            `quotRem` (588 `div` 28)
    initialChar = "?????????????????????????????????????????????????????????"    !! traceIt "initial" initial
    medialChar  = "???????????????????????????????????????????????????????????????" !! traceIt "medial"  medial
    finalChar   = case traceIt "final" final of
                    0          -> Nothing
                    otherFinal -> Just $ " ?????????????????????????????????????????????????????????????????????????????????" !! final

-- | Take a Korean syllable Jamo and romanize it.
--   Non-Korean characters should pass through.
--
--   Ignores phonological changes.
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
romajanizeVowel '???' = "a"
romajanizeVowel '???' = "eo"
romajanizeVowel '???' = "o"
romajanizeVowel '???' = "u"
romajanizeVowel '???' = "eu"
romajanizeVowel '???' = "i"
romajanizeVowel '???' = "ae"
romajanizeVowel '???' = "e"
romajanizeVowel '???' = "ya"
romajanizeVowel '???' = "yeo"
romajanizeVowel '???' = "yo"
romajanizeVowel '???' = "yu"
romajanizeVowel '???' = "yae"
romajanizeVowel '???' = "ye"
romajanizeVowel '???' = "wa"
romajanizeVowel '???' = "oe"
romajanizeVowel '???' = "wae"
romajanizeVowel '???' = "wo"
romajanizeVowel '???' = "wi"
romajanizeVowel '???' = "we"
romajanizeVowel '???' = "ui"
romajanizeVowel  c   = [c]


-- | Position of the Korean consonant withing syllable character.
data ConsonantPos = Initial | Final

-- | Take a consonant and position, and return a romanization of it.
--   Other characters are let pass through.
--
--   TODO: Ask Korean speaker about romanization of: ?????????????????????. Same for ????????????
--         Should one first decompose these into ??? and another final consonant?
--         Should one apply phonological rules to these
--
--   TODO: Make standard conversion to non-composable Jamos.
romajanizeConsonant :: ConsonantPos -> Char -> String
romajanizeConsonant Initial '???' = "g" 
romajanizeConsonant Final   '???' = "k"
romajanizeConsonant Initial '???' = "kk"
romajanizeConsonant Final   '???' = "k"
romajanizeConsonant _       '???' = "n"
romajanizeConsonant Initial '???' = "d"
romajanizeConsonant Final   '???' = "t"
romajanizeConsonant Initial '???' = "tt"
romajanizeConsonant Final   '???' = "" -- TODO: or no character?
romajanizeConsonant Initial '???' = "r"
romajanizeConsonant Final   '???' = "l"
romajanizeConsonant _       '???' = "m"
romajanizeConsonant Initial '???' = "b"
romajanizeConsonant Final   '???' = "p"
romajanizeConsonant Initial '???' = "pp"
romajanizeConsonant Final   '???' = ""
romajanizeConsonant Initial '???' = "s"
romajanizeConsonant Final   '???' = "t"
romajanizeConsonant Initial '???' = "ss"
romajanizeConsonant Final   '???' = "t"
romajanizeConsonant Initial '???' = ""
romajanizeConsonant Final   '???' = "ng"
romajanizeConsonant Initial '???' = "j"
romajanizeConsonant Final   '???' = "t"
romajanizeConsonant Initial '???' = "jj"
romajanizeConsonant Final   '???' = ""
romajanizeConsonant Initial '???' = "ch"
romajanizeConsonant Final   '???' = "t"
romajanizeConsonant _       '???' = "k"
romajanizeConsonant _       '???' = "t"
romajanizeConsonant _       '???' = "p"
romajanizeConsonant Initial '???' = "h"
romajanizeConsonant Final   '???' = "t"
romajanizeConsonant pos     '???' = romajanizeConsonant pos '???'
romajanizeConsonant pos     '???' = romajanizeConsonant pos '???'
romajanizeConsonant pos     '???' = romajanizeConsonant pos '???'
romajanizeConsonant pos     '???' = romajanizeConsonant pos '???'
romajanizeConsonant pos     '???' = romajanizeConsonant pos '???'
romajanizeConsonant pos     '???' = romajanizeConsonant pos '???'
romajanizeConsonant pos     '???' = romajanizeConsonant pos '???'
romajanizeConsonant pos     '???' = romajanizeConsonant pos '???'
romajanizeConsonant pos     '???' = romajanizeConsonant pos '???'
romajanizeConsonant pos     '???' = romajanizeConsonant pos '???'
romajanizeConsonant pos     '???' = romajanizeConsonant pos '???'
romajanizeConsonant _        c  = [c]

-- | Romajanize modern composable Jamos
--
--   Modern Jamo character set distinguishes between initial
--   and final vowels. A knowledgeable Korean implementor
--   could use it to make romaja conversion without using
--   @ConsonantPos datatype, but I am too scared that I get it wrong.
--
--   Check if the glyphs can be converted algorithmically here.
romajanizeJamo :: Char -> String
-- Initial consonant Jamos
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
romajanizeJamo '???' = romajanizeConsonant Initial '???'
-- Final consonant Jamos
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
--romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
romajanizeJamo '???' = romajanizeConsonant Final '???'
-- Medial vowel Jamos
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
romajanizeJamo '???' = romajanizeVowel '???' 
--romajanizeJamo '???' = romajanizeVowel '???' <> romajanizeVowel '???'
--romajanizeJamo '\4514' = "??" -- double dot on top
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
phonology '???' '???' = "g"
phonology '???' '???' = "ngn"
phonology '???' '???' = "ngn"
phonology '???' '???' = "ngm"
phonology '???' '???' = "kh" -- instead of "h"
phonology '???' '???' = "nn"
phonology '???' '???' = "d" -- or "j"
phonology '???' '???' = "nn"
phonology '???' '???' = "nn"
phonology '???' '???' = "nm"
phonology '???' '???' = "th" -- or "t", "ch"
phonology '???' '???' = "r"
phonology '???' '???' = "mn"
phonology '???' '???' = "b"
phonology '???' '???' = "mn"
phonology '???' '???' = "mn"
phonology '???' '???' = "mm"
phonology '???' '???' = "ph" -- "p"
phonology '???' '???' = "s"
phonology '???' '???' = "nn"
phonology '???' '???' = "nn"
phonology '???' '???' = "nm"
phonology '???' '???' = "ngn"
phonology '???' '???' = "j"
phonology '???' '???' = "nn"
phonology '???' '???' = "nn"
phonology '???' '???' = "nm"
phonology '???' '???' = "th" -- "t", "ch"
phonology '???' '???' = "ch"
phonology '???' '???' = "nn"
phonology '???' '???' = "nn"
phonology '???' '???' = "nm"
phonology '???' '???' = "th" -- "t", "ch"
-- A bit off the beaten track
phonology '???' '???' = "h"
phonology '???' '???' = "k"
phonology '???' '???' = "nn"
phonology '???' '???' = "t"
phonology '???' '???' = "nn"
phonology '???' '???' = "nm"
phonology '???' '???' = "p"
phonology '???' '???' = "ch"
phonology '???' '???' = "t"
phonology  p   n   = romajanizeConsonant Final   p
                  <> romajanizeConsonant Initial n

-- | Romajanize a continuous string entirely in Korean alphabet.
romajanizeKoreanWord :: String -> String
romajanizeKoreanWord input =(          romajanizeConsonant Initial firstInitial
                          <> mconcat  (zipWith step decomposed $ tail decomposed)
                          <>           romajanizeVowel lastMedial
                          <> maybe "" (romajanizeConsonant Final) lastFinal)
  where
    firstInitial, lastMedial :: Char
    lastFinal :: Maybe Char
    (_, lastMedial, lastFinal) = last decomposed 
    decomposed :: [(Char, Char, Maybe Char)]
    decomposed = decomposeKoreanSyllableChar <$> input
    (firstInitial, _, _):_ = decomposed
    step :: (Char, Char, Maybe Char) -> (Char, Char, Maybe Char) -> String
    step (_, prevMedial, Just prevFinal) (nextInitial, _, _) =
         romajanizeVowel prevMedial
      <> phonology prevFinal nextInitial
    step (_, prevMedial, Nothing)        (nextInitial, _, _) =
      romajanizeVowel prevMedial <> romajanizeConsonant Initial nextInitial
  
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
romajanize = mconcat . go . T.unpack . normalize NFC . normalize NFKD . T.pack -- canonicalize Unicode
  where
    go []                                  = []
    go cs | isKoreanSyllableChar $ head cs = romajanizeKoreanWord aWord:go rest 
      where
        (aWord, rest) = span isKoreanSyllableChar cs
    go (c:cs) = romajanizeChar c:go cs
