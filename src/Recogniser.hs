{-# LANGUAGE BangPatterns #-}
module Recogniser
  (
    recogniser
  , makeMatcher
  , stringAsRegex
  , Tables(..)
  , nextState
  , mkTok
  , mkAcc
  ) where

import qualified Data.ByteString.Char8 as C8
import           Data.Char
import           Data.List
import qualified Data.Set    as Set
import           AbsSyn
import           CharSet
import           DFA
import           DFAMin
import           Output

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}
-- ---------------------------------------------------------------------

recogniser :: Tables -> C8.ByteString -> Set.Set (Accept Code)
recogniser t s = snd $ C8.foldl' step (0,Set.empty) s
  where
    step (st,ac) c = (st',Set.union ac ac')
      where
        (!st',!ac') = nextState t st (ord c)

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
    index = (base !! s) + a
  in
    if index > 0
      then
        if check !! index == a
          then ret $ table !! index
          else nextState t (deflt !! s) a
      else (0,Set.empty)

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
