{-# LANGUAGE OverloadedStrings     #-}
module DfaSpec where

import qualified Data.Map as Map
import           AbsSyn
import           CharSet
import           DFA
import           DFAMin
import           Info
import           NFA
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
        -- a = array (1,1) [(1,NSt [] [] [])]
      -- (scanner2nfa enc scanner startcodes) `shouldBe` a

        dfa = (minimizeDFA $ scanner2dfa enc scanner startcodes)
      putStrLn (infoDFA 1 "foo" dfa "")
      dfa `shouldBe` (DFA [0] Map.empty )


        -- DFA { dfa_start_states = [0]
        --     , dfa_states = fromList [(0,State {state_acc = [], state_out = fromList [(97,2)]})
        --                             ,(1,State {state_acc = [0], state_out = fromList []})
        --                             ,(2,State {state_acc = [1], state_out = fromList [(98,1)]})
        --                             ]
        --     }
  
      -- (infoDFA 1 "foo" dfa "") `shouldBe` ""
       -- min_dfa = minimizeDFA dfa
        -- DFA { dfa_start_states = [0]
        --     , dfa_states = fromList [(0,State {state_acc = [ ], state_out = fromList [(97,1),(98,2)]})
        --                             ,(1,State {state_acc = [0], state_out = fromList []})
        --                             ,(2,State {state_acc = [1], state_out = fromList []})
        --                             ]
        --     }
    -- ---------------------------------

mkTok :: RExp -> RECtx
mkTok re = RECtx { reCtxStartCodes = [("code1",1)]
                 , reCtxPreCtx = Nothing
                 , reCtxRE = re
                 , reCtxPostCtx = NoRightContext
                 , reCtxCode = Nothing
                 }
