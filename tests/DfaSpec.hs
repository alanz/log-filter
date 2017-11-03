{-# LANGUAGE OverloadedStrings     #-}
module DfaSpec where

import           Data.Char
import qualified Data.Map    as Map
import qualified Data.IntMap as IM
import qualified Data.Set    as Set
import           AbsSyn
import           CharSet
import           DFA
import           DFAMin
import           Info
import           Output
import           Recogniser
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
  describe "constructs a DFA low level" $ do
    -- ---------------------------------

    it "dfa 1" $ do
      let
        enc = UTF8
        toks = [ mkTok (stringAsRegex "ab")
               , mkTok (stringAsRegex "a")
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
      let t = Tables base table check deflt (map Set.fromList acc)
      nextState t 0 97 `shouldBe` (2,Set.fromList [mkAcc 1])
      nextState t 0 98 `shouldBe` (0,Set.fromList [])

      nextState t 1 97 `shouldBe` (0,Set.fromList [])
      nextState t 1 98 `shouldBe` (0,Set.fromList [])

      nextState t 2 97 `shouldBe` (0,Set.fromList [])
      nextState t 2 98 `shouldBe` (1,Set.fromList [mkAcc 0])

    -- ---------------------------------

    it "dfa 2" $ do
      let
        enc = UTF8
        toks = [ mkTok (stringAsRegex "2017110200002")
               ]
        scanner = Scanner "dfa2" toks
        startcodes = [1]

        dfa = (minimizeDFA $ scanner2dfa enc scanner startcodes)
      putStrLn (infoDFA 1 "dfa2" dfa "")

      let (base,table,check,deflt,acc) = mkTables dfa
      base `shouldBe` [-49,-47,-46,-51,-44,-42,-41,-40,-39,-37,-36,-35,-34,0]
      length table `shouldBe` 222
      length check `shouldBe` 222
      deflt `shouldBe` [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]
      acc `shouldBe` [[],[],[],[],[],[],[],[],[],[],[],[],[],[mkAcc 0]]

      let s = 0
          a = ord 'a'
      a `shouldBe` 97
      (base !! s) + a `shouldBe` 97 - 49
      let t = Tables base table check deflt (map Set.fromList acc)
      nextState t 0 97        `shouldBe` (0,Set.fromList [])
      nextState t 0 (ord '2') `shouldBe` (5,Set.fromList [])
      -- nextState t 0 (ord '-') `shouldBe` (5,Set.fromList [])
      let a = ord '-'
      (base !! s) + a `shouldBe` -4


    -- ---------------------------------

  describe "constructs a DFA using helper" $ do
    -- ---------------------------------

    it "dfa 1" $ do

      let t = makeMatcher ["ab","a"]

      nextState t 0 97 `shouldBe` (2,Set.fromList [mkAcc 1])
      nextState t 0 98 `shouldBe` (0,Set.fromList [])

      nextState t 1 97 `shouldBe` (0,Set.fromList [])
      nextState t 1 98 `shouldBe` (0,Set.fromList [])

      nextState t 2 97 `shouldBe` (0,Set.fromList [])
      nextState t 2 98 `shouldBe` (1,Set.fromList [mkAcc 0])

    -- ---------------------------------

    it "runs the recogniser on simple case" $ do

      let t = makeMatcher ["ab","a"]
      recogniser t ""          `shouldBe` Set.empty
      recogniser t "a"         `shouldBe` Set.fromList [mkAcc 1]
      recogniser t "fa"        `shouldBe` Set.fromList [mkAcc 1]
      recogniser t "ab"        `shouldBe` Set.fromList [mkAcc 1,mkAcc 0]
      recogniser t "xxxabcdea" `shouldBe` Set.fromList [mkAcc 0,mkAcc 1]
    -- ---------------------------------

    it "runs the recogniser 2" $ do

      let t = makeMatcher ["2017110200002"]
      recogniser t ""          `shouldBe` Set.empty

      recogniser t "2017-"
                               `shouldBe` Set.empty

      recogniser t "2017-11-02T00:00:20.054+00,REQ,v4,D0E53F7EBF601E71884A001122334494,/deliver/w2mgmbh_naughtytube_1901_gh_billing_dlrproc,197.189.228.76:56268,200,0ms,username=w2mgh001&password=zoo3ooj5&to=1901&at=20171102000020&status=REJECTD&REF=1%2F55CE79D2D4530%2F155%2F490100035%2Fall%2FNaughty%20Girls%20-%20Clic&SYNREF=375A33BEBEDE1E71906F001122334471&from=233265710585&sub=001&err=107&dlvrd=000,OK%2CD0E53F7EBF601E71884A001122334494,https"
                               `shouldBe` Set.fromList [mkAcc 0]

-- ---------------------------------------------------------------------
