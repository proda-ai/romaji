{-# LANGUAGE Haskell2010#-}
module Data.Char.Romaja
    ( isKoreanChar
    , isLatinChar
    , romajanize
    , romajanizeChar
    , decomposeKoreanSyllableChar
    ) where

import Data.Char

import Debug.Trace

traceIt :: Show a => [Char] -> a -> a
traceIt msg val = trace (msg <> " " <> show val) val 

isKoreanChar :: Char -> Bool
isKoreanChar c = (ord c >= 0x1100 && ord c <= 0x11FF) -- Hangul Jamo
              || (ord c >= 0x3130 && ord c <= 0x318F) -- Hangul extended Jamo
              || (ord c >= 0xA960 && ord c <= 0xA97F) -- Hangul extended Jamo A
              || (ord c >= 0xD7B0 && ord c <= 0xD7FF) -- Hangul extended Jamo B
              || (ord c >= 0x3200 && ord c <= 0x321E) -- Enclosed CJK letters and months 1 (parenthesised)
              || (ord c >= 0x3260 && ord c <= 0x327F) -- Enclosed CJK letters and months 2 (circled)
              || (ord c >= 0xFFA0 && ord c <= 0xFFDC) -- Half-width Hangul compatibility
              || isKoreanSyllableChar c

isKoreanSyllableChar  :: Char -> Bool
isKoreanSyllableChar c = ord c >= 0xAC00 && ord c <= 0xD7A3 -- Hangul syllables

decomposeKoreanSyllableChar c = [initialChar, medialChar] <> finalChar 
  where
    (rest   , final ) = (traceIt "a" (ord c - 44032)) `quotRem`  28
    (initial, medial) = (traceIt "b" rest           ) `quotRem` (588 `div` 28)
    initialChar = "ㄱㄲㄴㄷㄸㄹㅁㅂㅃㅅㅆㅇㅈㅉㅊㅋㅌㅍㅎ"    !! traceIt "initial" initial
    medialChar  = "ㅏㅐㅑㅒㅓㅔㅕㅖㅗㅘㅙㅚㅛㅜㅝㅞㅟㅠㅡㅢㅣ" !! traceIt "medial"  medial
    finalChar   = case traceIt "final" final of
                    0 -> []
                    otherFinal -> [" ㄱㄲㄳㄴㄵㄶㄷㄹㄺㄻㄼㄽㄾㄿㅀㅁㅂㅄㅅㅆㅇㅈㅊㅋㅌㅍㅎ" !! final]

isLatinChar :: Char -> Bool
isLatinChar = isLatin1

romajanizeChar :: Char -> String
romajanizeChar  = undefined

romajanize :: String -> String 
romajanize = undefined

someFunc :: IO ()
someFunc = putStrLn "someFunc"
