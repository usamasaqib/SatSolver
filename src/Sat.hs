{-
This module implements the dpll algorithm.
Each branch of the assignment tree can be regarded as a bitmap, with each bit representing the Truth value of the literal
corresponding in position in the lookup table. I.e. bit: 5 represents the literal indexed by the number 5.
The algorithm checks whether the assignments specified by the bitmap satisfy the expression.
If so then the algorithm terminates. 
If not then using the following heuristic the algorithm calculates the next branch to visit and repeat the assignment process.
The branches are visited in left to right order. I.e branch 1 = [False, False .... False] and the last branch is [True, True .... True]
 -- If the failed variable had a False assignment then we toggle this to true and set all subsequent bits to False.
 -- If the failed variable has a True assignment then we find the first bit that had a False assignment, HIGHER up in the tree and set it to True, and
    all subsequent bits to False.
    - if no such bit is found then the proposition has no possible solution and the algorithm terminates, indicating faliure.

It can seen from the description how this algorithm implements search space reduction. Simply put of the assignment branch fails at any point
there is no need to continue down that branch. Instead we skip to the next branch.

On top of this the algorithm also implements unit propogation in order to speed up the assignment process.
-}

module Sat where

import Control.Applicative
import Data.List.Split (splitOn)

data Status = SAT | UnSAT | UnResolved deriving (Eq, Show)

data Literal a = Simply a | Not a deriving (Eq, Show)

data Clause a = Cl a | Unit a | Satisfied deriving Show

type Expr a = [Clause [Literal a]]

type ExprHistory a = [Expr a]

type CurrentVar = Int

type TotalNumOfVars = Int

type FailedVarID = Int

type VarLookUp a = [(Int, Literal a)]

type LiteralPresent = Bool

type NegativePresent = Bool

type Contains = (LiteralPresent, NegativePresent)

instance Functor Clause where
    fmap _ Satisfied = Satisfied
    fmap trans (Cl a) = Cl (trans a)
    fmap trans (Unit a) = Unit (trans a)

instance Applicative Clause where
    pure = Cl
    _ <*> Satisfied = Satisfied
    (Cl func) <*> (Cl a) = Cl $ func a

instance Monad Clause where
    return = pure
    Satisfied >>= _ = Satisfied
    clause >>= func = func (evalClause clause)

evalClause :: Clause a -> a
evalClause (Cl a) = a
evalClause (Unit a) = a


mkClause :: a -> Clause a
mkClause = Cl

inverseLiteral :: Literal a -> Literal a
inverseLiteral (Simply x) = Not x
inverseLiteral (Not lit) = Simply lit

containsLiteral :: Eq a => Clause [Literal a] -> Literal a -> Contains
containsLiteral Satisfied _ = (False, False)
containsLiteral clause checkLit = 
    ( evalClause $ fmap (elem checkLit) clause ,
      evalClause $ fmap (elem (inverseLiteral checkLit)) clause 
    )

filterClause :: Eq a => Literal a -> [Literal a] -> Clause [Literal a]
filterClause lit litLs= mkClause $ filter (/= lit) litLs


{-
Cases that simplify clause checks.

 BoolValue | Clause Containes | Operation
 --------- |  --------------- | ---------
  True     |   Inverse (lit)  |  filter the literal
  True     |      lit         |  return [], since clause is satisfied
  False    |      lit         |  filter the literal
  False    |   Inverse (lit)  |  return [], since clause is satisfied  
-}
simplifyClause :: Eq a => Literal a -> Bool -> Contains -> Clause [Literal a] -> Maybe (Clause [Literal a])
-- The following two are success cases, where a clause is satisfied
simplifyClause lit True (True, _) clause  = Just $ clause >>= const Satisfied
simplifyClause lit False (_, True) clause = Just $ clause >>= const Satisfied

--The following two are cases where a clause maybe unsatisfied. An unsatisfied clause is Cl []
simplifyClause lit True (False, True)  clause  = 
    if null (evalClause cl) 
        then Nothing 
    else Just cl
 where cl = clause >>= filterClause (inverseLiteral lit)

simplifyClause lit False (True, False) clause  = 
    if null (evalClause cl) 
        then Nothing 
    else Just cl
 where cl = clause >>= filterClause lit

-- All other cases
simplifyClause lit _ _ clause = Just clause


simplifyExpression :: Eq a => Literal a -> Bool -> Expr a -> Maybe (Expr a)
simplifyExpression lit assign expression = sequence simplification
 where simplification = map (\cl -> 
                            simplifyClause lit assign (containsLiteral cl lit) cl) expression


-- A unit is identified as a clause containing only one literal.
findUnit :: Eq a => Maybe (Expr a) -> [Maybe [Literal a]] -> Maybe [Literal a]
findUnit Nothing _ = Nothing
findUnit (Just []) ans = if null (evalMaybe seq) then Nothing else seq
 where seq = fmap concat (sequence ans)
       evalMaybe (Just x) = x
findUnit (Just (cl:expr)) ans = 
    case isNewUnit cl of
        True -> findUnit (Just expr) ( (Just $ evalClause cl) : ans)
        False -> findUnit (Just expr) ans
 where isNewUnit (Cl a) = length a == 1
       isNewUnit _ = False 


-- Unit propogation is accomplished by taking a list of units found by the findUnits function.
-- Then simplifyExpressions function is called with the literal and a boolean assignment of True.
-- Once this is done we create Unit instances from the list of units and readd them to the list of clauses, i.e the expression.
unitPropogation :: Eq a => Maybe [Literal a] -> Maybe (Expr a) -> Maybe (Expr a)
unitPropogation Nothing expr = expr
unitPropogation _ Nothing = Nothing
unitPropogation (Just units) expr = fmap (reAddUnits ++) (unitPropogate expr units)
 where unitPropogate :: Eq a => Maybe [Clause [Literal a]] -> [Literal a] -> Maybe [Clause [Literal a]]
       unitPropogate Nothing _ = Nothing
       unitPropogate (Just expr) [] = Just expr
       unitPropogate (Just expr) (u:unit) = unitPropogate (simplifyExpression u True expr) unit 
       reAddUnits = map (\x -> Unit [x]) units


simplifyAndRulesApply :: Eq a => Expr a -> Bool -> Literal a -> Maybe (Expr a)
simplifyAndRulesApply expr assignment lit = unitPropogation (findUnit simplified []) simplified
 where simplified = simplifyExpression lit assignment expr


-- Assigns literals to the boolean values given in the list provided.
decisionBranch :: Eq a => [Bool] -> CurrentVar -> VarLookUp a -> Maybe (Expr a) -> Either Status (FailedVarID, [Expr a] )
decisionBranch _ failedVar _ Nothing = Right (failedVar - 1, [[]]) -- subtract one becase failiure actually happened on the previous literal. 
decisionBranch [] _ _ _ = Left SAT
decisionBranch (assign:assignLs) curVar table (Just expr) = 
    -- Expr is the state of the expression, before it is simplified.
    -- We concatenate this into our failiure report, so the appropriate
    -- simplification can be passed in once the next literal value to be modified 
    -- is calculated.
    recurProc >>= (Right . fmap (expr :) )
 where recurProc = decisionBranch assignLs (curVar+1) table simplified
       simplified = simplifyAndRulesApply expr assign (fromJust (lookup curVar table))
       fromJust (Just x) = x


-- If the the literal is False, then the new branch is the same
-- except for that the new literal value is True, and all subsequent literals are set to False.
-- If the literal is True, then the first Literal higher up in the Tree, with a value of False
-- is found. Its value is set to True and all subsequent literals in the branch are set to False.
calculateNextBranch :: [Bool] -> CurrentVar -> TotalNumOfVars -> ([Bool], Int)
calculateNextBranch branch size numOfVar = 
    if (foldr (&&) True branch == True) || (foldr (||) False (fst newTree) == False)
        then ([], numOfVar)
    else newTree
 where toggled = toggleBit (take size branch) size
       newTree = (fst toggled ++ replicate (numOfVar - size) False, snd toggled - 1)


-- The boolean list, may be regarded as a bitmap. In that sense this function toggles the appropriate bit.
toggleBit :: [Bool] -> Int -> ([Bool], Int)
toggleBit branch size = (reverse boolList, size - length (takeWhile not boolList) )
 where toggled [] = []
       toggled ls = 
        if not (head ls)
            then True : tail ls
        else False : toggled (tail ls)
       boolList = toggled (reverse branch)


-- This function calls decisionBranch, and if decisionBranch succeeds then it returns, the list of boolean values that satisfy the proposition.
-- Otherwise, the function calculates the next branch to visit based on the faliure information returned by decisionBranch.
-- ExprHistory here represents the list containing the evaluated state of the expression. The reason this is mantained is so that
-- the expression does not have to be unnecessarily reevaluated.
sat :: Eq a => Expr a -> ExprHistory a -> [Bool] -> CurrentVar -> TotalNumOfVars -> VarLookUp a -> Maybe [Bool]
sat _ _ [] _ _ _ = Nothing
sat expr exprState impliedTreeBranch var numOfVar table = 
    case result of
        (Left _) -> Just impliedTreeBranch
        (Right (failiure, satisfiedSoFar)) ->
            let (next, varModified) = nextBranch failiure 
                recursionDepth = varModified - var
                newExprState = take varModified exprState in
                    if recursionDepth < 0
                        -- This case is for when a variable higher up in the tree is changed. The error report
                        -- from decision branch thus does not contain the correct state of the expression.
                        -- We must look in the saved history from before. 
                        then sat (exprState !! varModified) newExprState next (varModified + 1) numOfVar table
                        -- As we progress down the tree. The current resolved state of the expression is added to the
                        -- history of the expression. This is done in the if stmt.
                    else sat (satisfiedSoFar !! recursionDepth ) (
                        if recursionDepth == 0 then exprState else exprState ++ satisfiedSoFar ) next (varModified + 1) numOfVar table 
 where result = decisionBranch branchToTest var table (Just expr)
       nextBranch failPoint = calculateNextBranch impliedTreeBranch failPoint numOfVar
       branchToTest = drop (var - 1) impliedTreeBranch

callSat expr = sat expr []  
