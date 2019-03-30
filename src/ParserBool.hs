{-
This parser was implemented by following the 
chapter on Monadic Parsing in Graham Huttons book Programming in Haskell.

NOTE: This module uses the hatt library in order to convert the given
boolean proposition into cnf form.
-}

module ParserBool where

import Control.Applicative
import Data.Char
import Data.Logic.Propositional
import Data.Logic.Propositional.NormalForms

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
    -- fmap :: (a -> b) -> f a -> f b
    fmap func inPars = P (\inp -> case parse inPars inp of
                                   [] -> []
                                   [(v, out)] -> [(func v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure param = P (\inp -> [(param, inp)])

    -- <*> :: Parse (a -> b) -> Parser a -> Parser b
    pFunc <*> inPars = P (\inp -> case parse  pFunc inp of
                                   [] -> []
                                   [(func, out)] -> parse (fmap func inPars) out)

instance Monad Parser where
    --return :: a -> Parser a
    return = pure

    -- (>>=) :: m a -> (a -> m b) -> m b
    inPars >>= mFunc = P (\inp ->
        case parse inPars inp of
            [] -> []
            [(v, out)] -> parse (mFunc v) out)

instance Alternative Parser where
    --empty :: Parser a
    empty = P (\inp ->  [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> 
        case parse p inp of
            [] -> parse q inp
            [(v, out)] -> [(v, out)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                   [] -> []
                   (x:xs) -> [(x, xs)] )


-- derived primitives 
sat :: (Char -> Bool) -> Parser Char
sat p = 
    do
        x <- item
        if p x then return x else empty

letter :: Parser Char
letter = sat isAlpha

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = 
    do
        char x
        string xs
        return (x:xs)


space :: Parser ()
space = do
         many (sat isSpace)
         return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

symbol :: String -> Parser String
symbol xs = token (string xs)

expr :: Parser Expr
expr = 
    do 
        t <- term
        do symbol "|"
           e <- expr
           return (Disjunction t e)
           <|> return t
        
term :: Parser Expr
term = 
    do 
        f <- factor
        do symbol "&"
           t <- term
           return ( Conjunction f t)
           <|> return f

factor :: Parser Expr
factor = 
    do
        symbol "("
        e <- expr
        symbol ")"
        return e
        <|> do symbol "~"
               f <- factor
               return (Negation f) 
        <|> pure Variable <*> (pure Var <*> letter)


-- YOU SHOULD CONVERT TO CNF HERE
eval :: String -> String
eval prop = case (parse expr prop) of
    [(n, [])] -> showAscii (toCNF n)
    _ -> "Invalid Input"
