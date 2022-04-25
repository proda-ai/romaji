module Data.Char.Romaji where

-- ^ Kunrei-shiki romanization of Japanese. 

import Data.Char

-- | Romanization of single characters.
romajizeChar :: Char -> [Char]
-- Katakana
romajizeChar 'ア' = "a"
romajizeChar 'イ' = "i"
romajizeChar 'ウ' = "u" 
romajizeChar 'エ' = "e" 
romajizeChar 'オ' = "o"
romajizeChar 'カ' = "ka" 
romajizeChar 'キ' = "ki" 
romajizeChar 'ク' = "ku" 
romajizeChar 'ケ' = "ke" 
romajizeChar 'コ' = "ko" 
romajizeChar 'サ' = "sa" 
romajizeChar 'シ' = "si" 
romajizeChar 'ス' = "su" 
romajizeChar 'セ' = "se" 
romajizeChar 'ソ' = "so" 
romajizeChar 'タ' = "ta" 
romajizeChar 'チ' = "ti" 
romajizeChar 'ツ' = "tu" 
romajizeChar 'テ' = "te" 
romajizeChar 'ト' = "to" 
romajizeChar 'ナ' = "na" 
romajizeChar 'ニ' = "ni" 
romajizeChar 'ヌ' = "nu" 
romajizeChar 'ネ' = "ne" 
romajizeChar 'ノ' = "no" 
romajizeChar 'ハ' = "ha" 
romajizeChar 'ヒ' = "hi" 
romajizeChar 'フ' = "hu" 
romajizeChar 'ヘ' = "he" 
romajizeChar 'ホ' = "ho" 
romajizeChar 'マ' = "ma" 
romajizeChar 'ミ' = "mi" 
romajizeChar 'ム' = "mu" 
romajizeChar 'メ' = "me" 
romajizeChar 'モ' = "mo" 
romajizeChar 'ヤ' = "ya" 
romajizeChar 'ユ' = "yu" 
romajizeChar 'ヨ' = "yo" 
romajizeChar 'ラ' = "ra" 
romajizeChar 'リ' = "ri" 
romajizeChar 'ル' = "ru" 
romajizeChar 'レ' = "re" 
romajizeChar 'ロ' = "ro" 
romajizeChar 'ヰ' = "wi" 
romajizeChar 'ヱ' = "we" 
romajizeChar 'ヲ' = "wo" 
romajizeChar 'ン' = "n" -- "n'" when next is 'i' 
romajizeChar 'ガ' = "ga" 
romajizeChar 'ギ' = "gi" 
romajizeChar 'グ' = "gu" 
romajizeChar 'ゲ' = "ge" 
romajizeChar 'ゴ' = "go" 
romajizeChar 'ザ' = "za" 
romajizeChar 'ジ' = "zi" 
romajizeChar 'ズ' = "zu" 
romajizeChar 'ゼ' = "ze" 
romajizeChar 'ゾ' = "zo" 
romajizeChar 'ダ' = "da" 
romajizeChar 'ヂ' = "zi" 
romajizeChar 'ヅ' = "zu" 
romajizeChar 'デ' = "de" 
romajizeChar 'ド' = "do" 
romajizeChar 'バ' = "ba" 
romajizeChar 'ビ' = "bi" 
romajizeChar 'ブ' = "bu" 
romajizeChar 'ベ' = "be" 
romajizeChar 'ボ' = "bo" 
romajizeChar 'パ' = "pa" 
romajizeChar 'ピ' = "pi" 
romajizeChar 'プ' = "pu" 
romajizeChar 'ペ' = "pe" 
romajizeChar 'ポ' = "po" 
romajizeChar 'ヴ' = "vu"
-- Hiragana
romajizeChar 'あ' = "a"
romajizeChar 'い' = "i"
romajizeChar 'う' = "u" 
romajizeChar 'え' = "e" 
romajizeChar 'お' = "o"
romajizeChar 'か' = "ka" 
romajizeChar 'き' = "ki" 
romajizeChar 'く' = "ku" 
romajizeChar 'け' = "ke" 
romajizeChar 'こ' = "ko" 
romajizeChar 'さ' = "sa" 
romajizeChar 'し' = "si" 
romajizeChar 'す' = "su" 
romajizeChar 'せ' = "se" 
romajizeChar 'そ' = "so" 
romajizeChar 'た' = "ta" 
romajizeChar 'ち' = "ti" 
romajizeChar 'つ' = "tu" 
romajizeChar 'て' = "te" 
romajizeChar 'と' = "to" 
romajizeChar 'な' = "na" 
romajizeChar 'に' = "ni" 
romajizeChar 'ぬ' = "nu" 
romajizeChar 'ね' = "ne" 
romajizeChar 'の' = "no" 
romajizeChar 'は' = "ha" 
romajizeChar 'ひ' = "hi" 
romajizeChar 'ふ' = "hu" 
romajizeChar 'へ' = "he" 
romajizeChar 'ほ' = "ho" 
romajizeChar 'ま' = "ma" 
romajizeChar 'み' = "mi" 
romajizeChar 'む' = "mu" 
romajizeChar 'め' = "me" 
romajizeChar 'も' = "mo" 
romajizeChar 'や' = "ya" 
romajizeChar 'ゆ' = "yu" 
romajizeChar 'よ' = "yo" 
romajizeChar 'ら' = "ra" 
romajizeChar 'り' = "ri" 
romajizeChar 'る' = "ru" 
romajizeChar 'れ' = "re" 
romajizeChar 'ろ' = "ro" 
romajizeChar 'ゐ' = "i" 
romajizeChar 'ゑ' = "e" 
romajizeChar 'を' = "o" 
romajizeChar 'ん' = "n" -- n', when next is 'i' 
romajizeChar 'が' = "ga" 
romajizeChar 'ぎ' = "gi" 
romajizeChar 'ぐ' = "gu" 
romajizeChar 'げ' = "ge" 
romajizeChar 'ご' = "go" 
romajizeChar 'ざ' = "za" 
romajizeChar 'じ' = "zi" 
romajizeChar 'ず' = "zu" 
romajizeChar 'ぜ' = "ze" 
romajizeChar 'ぞ' = "zo" 
romajizeChar 'だ' = "da" 
romajizeChar 'ぢ' = "zi" 
romajizeChar 'づ' = "zu" 
romajizeChar 'で' = "de" 
romajizeChar 'ど' = "do" 
romajizeChar 'ば' = "ba" 
romajizeChar 'び' = "bi" 
romajizeChar 'ぶ' = "bu" 
romajizeChar 'べ' = "be" 
romajizeChar 'ぼ' = "bo" 
romajizeChar 'ぱ' = "pa" 
romajizeChar 'ぴ' = "pi" 
romajizeChar 'ぷ' = "pu" 
romajizeChar 'ぺ' = "pe" 
romajizeChar 'ぽ' = "po" 
romajizeChar 'ゔ' = "vu"
romajizeChar 'ー' = "\770" -- long vowel changed to macron
romajizeChar  o   = [o] -- pass through

-- | Romajization of pairs of characters:
romajizePair :: Char -> Char -> [Char]
-- Katakana
romajizePair 'キ' 'ャ' = "kya"  
romajizePair 'キ' 'ュ' = "kyu" 
romajizePair 'キ' 'ョ' = "kyo"
romajizePair 'シ' 'ャ' = "sya"
romajizePair 'シ' 'ュ' = "syu"
romajizePair 'シ' 'ョ' = "syo"
romajizePair 'チ' 'ャ' = "tya"
romajizePair 'チ' 'ュ' = "tyu"
romajizePair 'チ' 'ョ' = "tyo"
romajizePair 'ニ' 'ャ' = "nya"
romajizePair 'ニ' 'ュ' = "nyu"
romajizePair 'ニ' 'ョ' = "nyo"
romajizePair 'ヒ' 'ャ' = "hya"
romajizePair 'ヒ' 'ュ' = "hyu"
romajizePair 'ヒ' 'ョ' = "hyo"
romajizePair 'ミ' 'ャ' = "mya"
romajizePair 'ミ' 'ュ' = "myu"
romajizePair 'ミ' 'ョ' = "myo"
romajizePair 'リ' 'ャ' = "rya"
romajizePair 'リ' 'ュ' = "ryu"
romajizePair 'リ' 'ョ' = "ryo"
romajizePair 'ギ' 'ャ' = "gya"
romajizePair 'ギ' 'ュ' = "gyu"
romajizePair 'ギ' 'ョ' = "gyo"
romajizePair 'ジ' 'ャ' = "zya"
romajizePair 'ジ' 'ュ' = "zyu"
romajizePair 'ジ' 'ョ' = "zyo"
romajizePair 'ヂ' 'ャ' = "zya"
romajizePair 'ヂ' 'ュ' = "zyu"
romajizePair 'ヂ' 'ョ' = "zyo"
romajizePair 'ビ' 'ャ' = "bya"
romajizePair 'ビ' 'ュ' = "byu"
romajizePair 'ビ' 'ョ' = "byo"
romajizePair 'ピ' 'ャ' = "pya"
romajizePair 'ピ' 'ュ' = "pyu"
romajizePair 'ピ' 'ョ' = "pyo"
-- Hiragana
romajizePair 'き' 'ゃ' = "kya"  
romajizePair 'き' 'ゅ' = "kyu" 
romajizePair 'き' 'ょ' = "kyo"
romajizePair 'し' 'ゃ' = "sya"
romajizePair 'し' 'ゅ' = "syu"
romajizePair 'し' 'ょ' = "syo"
romajizePair 'ち' 'ゃ' = "tya"
romajizePair 'ち' 'ゅ' = "tyu"
romajizePair 'ち' 'ょ' = "tyo"
romajizePair 'に' 'ゃ' = "nya"
romajizePair 'に' 'ゅ' = "nyu"
romajizePair 'に' 'ょ' = "nyo"
romajizePair 'ひ' 'ゃ' = "hya"
romajizePair 'ひ' 'ゅ' = "hyu"
romajizePair 'ひ' 'ょ' = "hyo"
romajizePair 'み' 'ゃ' = "mya"
romajizePair 'み' 'ゅ' = "myu"
romajizePair 'み' 'ょ' = "myo"
romajizePair 'り' 'ゃ' = "rya"
romajizePair 'り' 'ゅ' = "ryu"
romajizePair 'り' 'ょ' = "ryo"
romajizePair 'ぎ' 'ゃ' = "gya"
romajizePair 'ぎ' 'ゅ' = "gyu"
romajizePair 'ぎ' 'ょ' = "gyo"
romajizePair 'じ' 'ゃ' = "zya"
romajizePair 'じ' 'ゅ' = "zyu"
romajizePair 'じ' 'ょ' = "zyo"
romajizePair 'ぢ' 'ゃ' = "zya"
romajizePair 'ぢ' 'ゅ' = "zyu"
romajizePair 'ぢ' 'ょ' = "zyo"
romajizePair 'び' 'ゃ' = "bya"
romajizePair 'び' 'ゅ' = "byu"
romajizePair 'び' 'ょ' = "byo"
romajizePair 'ぴ' 'ゃ' = "pya"
romajizePair 'ぴ' 'ゅ' = "pyu"
romajizePair 'ぴ' 'ょ' = "pyo"


romajize :: [Char] -> [Char]
romajize = concat . map romajizeChar