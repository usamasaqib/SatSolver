{-
 This file basically forms a bridge between the output of the parser and 
 the input of the dpll algorithm.
 This recasts the data so that it can be passed to the dpll algorithm
-}

module Solver 
 (
    evaluate
  , parse
 ) where

import qualified ParserBool as Pb
import Sat
import Data.Ord (comparing)
import Data.List (sort, sortBy, group)
import Data.List.Split (splitOn)
import Control.Arrow ((&&&))

type Vars = String

constructExpr :: [String] -> ( [Sat.Clause [Sat.Literal Char]], Vars )
constructExpr strLs = ( map (Sat.mkClause . constructClause) filterSymbols, allVars)
 where filterSymbols = map (filter (`notElem` "()|")) strLs
       constructClause [] = []
       constructClause (' ':str) = constructClause str
       constructClause ('~':x:str) = Sat.Not x : constructClause str
       constructClause (x:str) = Sat.Simply x : constructClause str
       allVars = filter (\x -> x `elem` ['a'..'z']) (concat filterSymbols)

giveVarIds :: Int -> [(a, Int)] -> [(Int, a)]
giveVarIds numOfVars vars = zip [1..numOfVars] (map fst vars)

evaluate :: String -> Maybe [(Char, Bool)]
evaluate prop = satisfiedRes >>= (\x -> Just (orderedVars `zip` x)) 
 where exprStr = splitOn "&" prop -- $ Pb.eval prop
       (expr, vars) = constructExpr exprStr
       -- sorts the literal lookup table based on the frequency of the variables.
       -- This is done so that the literals with the highest frequency are assigned
       -- first, thereby possibly reducing the depth of the tree traversed.
       table = sortBy (flip $ comparing snd) $ 
                map (head &&& length) $ group . sort $ vars
       literalTable = map (fmap Sat.Simply) (giveVarIds numOfVars table)
       start = replicate numOfVars False
       currVar = 1
       numOfVars = length table
       satisfiedRes = Sat.callSat expr start currVar numOfVars literalTable
       orderedVars = map (\tup -> fst tup) table

parse :: String -> String
parse = Pb.eval



