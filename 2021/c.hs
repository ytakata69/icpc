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
        print . solve $ convert tree
        main

solve :: MinMaxTree -> Int
solve tree@(Node opv children) =
    max rootv . maximum . map solve' $ rotations children
    where rootv = getMax tree
          solve' (c : cs) = chroot (minMax (opval opv) cs) c

-- compute the max value when a specified non-root node is made the root
-- (pmin, pmax): min and max value of the parent when it becomes a child
chroot :: (Int, Int) -> MinMaxTree -> Int
chroot _ (Node _ []) = minBound
chroot (pmin, pmax) (Node opv children) =
    max rootv . maximum . map chroot' $ rotations children
    where pnode = Node (MinMax {opval = Val 0, minv = pmin, maxv = pmax}) []
          rootv = snd $ minMax (opval opv) (pnode : children)
          chroot' (c : cs) = chroot (minMax (opval opv) (pnode : cs)) c

-- utilities
-- rotations [1,2,3] => [[1,2,3], [2,3,1], [3,1,2]]
rotations :: [a] -> [[a]]
rotations xs = take l . map (take l) . iterate tail $ cycle xs
    where l = length xs

-- trees with the min and max values

data Tree a = Node a [Tree a]  deriving (Show)
data MinMax = MinMax { opval :: ParseLabel, minv :: Int, maxv :: Int }
              deriving (Show)
type MinMaxTree = Tree MinMax

getMin :: MinMaxTree -> Int
getMin (Node mm _) = minv mm
getMax :: MinMaxTree -> Int
getMax (Node mm _) = maxv mm

convert :: ParseTree -> MinMaxTree
convert (Node opval children) =
    let (mn, mx) = minMax opval cs
    in  Node (MinMax {opval = opval, minv = mn, maxv = mx}) cs
    where cs = map convert children

minMax :: ParseLabel -> [MinMaxTree] -> (Int, Int)
minMax opval cs =
    case opval of
        Op Plus  -> (sumMin cs, sumMax cs)
        Op Minus -> (minimum $ map minMinus rotcs,
                     maximum $ map maxMinus rotcs)
        Val v    -> (v, v)
    where rotcs = rotations cs
          sumMin = sum . map getMin
          sumMax = sum . map getMax
          minMinus (hd : tl) = getMin hd - sumMax tl
          maxMinus (hd : tl) = getMax hd - sumMin tl

-- Parser

data Op = Plus | Minus  deriving (Eq, Show)
data ParseLabel = Op Op | Val Int  deriving (Show)
type ParseTree = Tree ParseLabel
type Parser = State String

parse :: String -> ParseTree
parse str = fst $ runState parseRoot str

parseRoot :: Parser ParseTree
parseRoot = do
    l  <- parseNonRoot
    op <- parseOp
    m  <- parseNonRoot
    _  <- parseOp      -- the same as op
    r  <- parseNonRoot
    return $ Node (Op op) [l, m, r]

parseNonRoot :: Parser ParseTree
parseNonRoot = do
    paren <- isNextChar '('
    if paren then do
        getOneChar  -- '('
        l  <- parseNonRoot
        op <- parseOp
        r  <- parseNonRoot
        getOneChar  -- ')'
        return $ Node (Op op) [l, r]
    else do
        v  <- parseVal
        return $ Node (Val v) []

parseOp :: Parser Op
parseOp = do
    ch <- getOneChar
    return $ if ch == '+' then Plus else Minus
parseVal :: Parser Int
parseVal = do
    ch <- getOneChar
    return $ read [ch]

getOneChar :: Parser Char
getOneChar = state $ \s -> (head s, tail s)
isNextChar :: Char -> Parser Bool
isNextChar ch = state $ \s -> (head s == ch, s)
