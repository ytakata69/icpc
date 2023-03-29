#!/usr/bin/env runghc

-- ACM-ICPC 2021 Domestic Qualifier
-- Problem C: Tree Transformation Puzzle

import Control.Monad (when)
import Control.Monad.State (State, state, runState)

main :: IO ()
main = do
    line <- getLine
    when (line /= "-1") $ do
        let tree = parse line
        print tree
        main

-- Parser

data Tree = Op Op [Tree] | Val Int  deriving (Show)
data Op = Plus | Minus  deriving (Eq, Show)
type Parser = State String

parse :: String -> Tree
parse str = fst $ runState parseRoot str

parseRoot :: Parser Tree
parseRoot = do
    l  <- parseNonRoot
    op <- parseOp
    m  <- parseNonRoot
    _  <- parseOp      -- the same as op
    r  <- parseNonRoot
    return $ Op op [l, m, r]

parseNonRoot :: Parser Tree
parseNonRoot = do
    paren <- isNextChar '('
    if paren then do
        getOneChar  -- '('
        l  <- parseNonRoot
        op <- parseOp
        r  <- parseNonRoot
        getOneChar  -- ')'
        return $ Op op [l, r]
    else
        parseVal

parseOp :: Parser Op
parseOp = do
    ch <- getOneChar
    return $ if ch == '+' then Plus else Minus
parseVal :: Parser Tree
parseVal = do
    ch <- getOneChar
    return $ Val (read [ch])

getOneChar :: Parser Char
getOneChar = state $ \s -> (head s, tail s)
isNextChar :: Char -> Parser Bool
isNextChar ch = state $ \s -> (head s == ch, s)
