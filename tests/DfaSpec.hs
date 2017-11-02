{-# LANGUAGE OverloadedStrings     #-}
module DfaSpec where

import qualified Data.Map    as Map
import qualified Data.IntMap as IM
import           AbsSyn
import           CharSet
import           DFA
import           DFAMin
import           Info
import           Output
-- import           NFA
-- import           Data.Array
-- import           Data.Ranged.Boundaries
-- import           Data.Ranged.RangedSet
-- import           Data.Ranged.Ranges
import           Test.Hspec

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}
-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

-- ---------------------------------------------------------------------

spec :: Spec
spec = do
  describe "constructs a DFA" $ do
    -- ---------------------------------

    it "dfa 1" $ do
      let
        enc = UTF8
        toks = [ mkTok ((Ch (charSetSingleton 'a')) :%% (Ch (charSetSingleton 'b')))
               , mkTok (Ch (charSetSingleton 'a'))
               ]
        scanner = Scanner "foo" toks
        startcodes = [1]

        dfa = (minimizeDFA $ scanner2dfa enc scanner startcodes)
      putStrLn (infoDFA 1 "foo" dfa "")
      dfa `shouldBe`
        (DFA { dfa_start_states = [0]
             , dfa_states = Map.fromList [(0,State {state_acc = [       ], state_out = IM.fromList [(97,2)]})
                                         ,(1,State {state_acc = [mkAcc 0], state_out = IM.fromList []})
                                         ,(2,State {state_acc = [mkAcc 1], state_out = IM.fromList [(98,1)]})
                                         ]
             })

      let (base,table,check,deflt,acc) = mkTables dfa
      base `shouldBe` [-96,0,-95]

      table `shouldBe`
        [0,2,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        ,0,0,0,0,0]
      length table `shouldBe` 161

      check `shouldBe`
        [-1,97,-1,98,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
        ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
        ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
        ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
        ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
        ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
        ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
        ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
        ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
        ,-1,-1,-1,-1,-1,-1,-1,-1]
      length check `shouldBe` 161

      deflt `shouldBe` [-1,-1,-1]
      acc `shouldBe` [[],[mkAcc 0],[mkAcc 1]]

      let s = 0
          a = 97
      (base !! s) + a `shouldBe` 1
      let t = Tables base table check deflt acc
      nextState t 0 97 `shouldBe` 2
      nextState t 0 98 `shouldBe` 2

    -- ---------------------------------

data Tables = Tables
    { tblBase :: [Int]
    , tblTable :: [Int]
    , tblCheck :: [Int]
    , tblDefault :: [Int]
    , tblAccept :: [[Accept Code]]
    } deriving Show

nextState :: Tables -> Int -> Int -> Int
nextState t s a =
  let
    Tables base table check deflt _accept = t
  in
    if check !! ((base !! s) + a) == a
      then table !! ((base !! s) + a)
      else nextState t (deflt !! s) a

mkTok :: RExp -> RECtx
mkTok re = RECtx { reCtxStartCodes = [("code1",1)]
                 , reCtxPreCtx     = Nothing
                 , reCtxRE         = re
                 , reCtxPostCtx    = NoRightContext
                 , reCtxCode       = Nothing
                 }

mkAcc :: Int -> Accept Code
mkAcc prio = Acc { accPrio     = prio
                 , accAction   = Nothing
                 , accLeftCtx  = Nothing
                 , accRightCtx = NoRightContext
                 }
