module Main where

import Control.Monad(forM_)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.Expectations
import Test.QuickCheck
import Data.Char.Romaja

newtype Korean = Korean String
  deriving (Show, Eq)

instance Arbitrary Korean where
  arbitrary = undefined


main :: IO ()
main = hspec $ do
  it "Detects Korean character" $
    isKoreanChar '년' `shouldBe` True
  it "Detects Latin character" $
    isLatinChar '년' `shouldBe` False
  it "Detects Latin character" $
    isLatinChar '년' `shouldBe` False
  it "Should romanize Korean character" $
    romajanize "항" `shouldBe` "hang"
  prop "Romanizes any Korean text" $
    \(Korean txt) -> forM_ (romajanize txt) (`shouldSatisfy` isLatinChar)


-- Example of Korean from http://columnist.org/parkk/infoage/romaniz.htm
--example = "91년에 프랑스 주간 시사잡지 몇 가지를 구독신청했더니 경쟁적으로 선물을 보내 주었다.그 가운데서 가장 요긴하게 쓴 것은 명함 크기 만한 전자수첩이었다.이 물건은 전화번호 1만 개를 기억시킬 수 있고 수표 발행 기록과 세 가지 신용카드의 사용 기록도 할 수 있어서 편리했다.다만 한국 사람 이름은 모두 로마자로 써서 입력해야 했으므로 내 나름의 표기법을 만들어 썼다.":w
