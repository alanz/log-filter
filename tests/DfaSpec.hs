{-# LANGUAGE OverloadedStrings     #-}
module DfaSpec where

import           Data.Char
import           Data.List
import qualified Data.Map    as Map
import qualified Data.IntMap as IM
import qualified Data.Set    as Set
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

    it "runs the recogniser" $ do

      let t = makeMatcher ["ab","a"]
      recogniser t ""          `shouldBe` Set.empty
      recogniser t "a"         `shouldBe` Set.fromList [mkAcc 1]
      recogniser t "fa"        `shouldBe` Set.fromList [mkAcc 1]
      recogniser t "ab"        `shouldBe` Set.fromList [mkAcc 1,mkAcc 0]
      recogniser t "xxxabcdea" `shouldBe` Set.fromList [mkAcc 0,mkAcc 1]

-- ---------------------------------------------------------------------

recogniser :: Tables -> String -> Set.Set (Accept Code)
recogniser t s = snd $ foldl' step (0,Set.empty) s
  where
    step (st,ac) c = (st',Set.union ac ac')
      where
        (st',ac') = nextState t st (ord c)

-- ---------------------------------------------------------------------

makeMatcher :: [String] -> Tables
makeMatcher ss = Tables base table check deflt acc'
  where
    enc = UTF8
    toks = map (mkTok . stringAsRegex) ss
    scanner = Scanner "foo" toks
    startcodes = [1]
    dfa = minimizeDFA $ scanner2dfa enc scanner startcodes
    (base,table,check,deflt,acc) = mkTables dfa
    acc' = map Set.fromList acc

-- ---------------------------------------------------------------------

stringAsRegex :: String -> RExp
stringAsRegex s = foldl' acc Eps s
  where
    acc re c = re :%% Ch (charSetSingleton c)

-- ---------------------------------------------------------------------

data Tables = Tables
    { tblBase    :: ![Int]
    , tblTable   :: ![Int]
    , tblCheck   :: ![Int]
    , tblDefault :: ![Int]
    , tblAccept  :: ![Set.Set (Accept Code)]
    } deriving Show

-- ---------------------------------------------------------------------

nextState :: Tables -> Int -> Int -> (Int,Set.Set (Accept Code))
nextState t s a
  | s < 0 = (0,Set.empty)
  | otherwise =
  let
    Tables base table check deflt accept = t
    ret ns = (ns,accept !! ns)
  in
    if check !! ((base !! s) + a) == a
      then ret $ table !! ((base !! s) + a)
      else nextState t (deflt !! s) a

-- ---------------------------------------------------------------------

mkTok :: RExp -> RECtx
mkTok re = RECtx { reCtxStartCodes = [("code1",1)]
                 , reCtxPreCtx     = Nothing
                 , reCtxRE         = re
                 , reCtxPostCtx    = NoRightContext
                 , reCtxCode       = Nothing
                 }

-- ---------------------------------------------------------------------

mkAcc :: Int -> Accept Code
mkAcc prio = Acc { accPrio     = prio
                 , accAction   = Nothing
                 , accLeftCtx  = Nothing
                 , accRightCtx = NoRightContext
                 }

-- ---------------------------------------------------------------------
