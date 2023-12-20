module Task1
    ( task1
    ) where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import System.Environment
import qualified Data.Map as M

sortOn f = map snd . sortBy (comparing fst) . map (f &&& id)

clip coord size = coord >= 0 && coord < size
valid size solution xy@(x, y) = and [clip x size, clip y size, isNothing (M.lookup xy solution)]
neighbors size solution xy = length . filter (valid size solution) $ sequence moves xy

moves = do
    f <- [(+), subtract]
    g <- [(+), subtract]
    (x, y) <- [(1, 2), (2, 1)]
    [f x *** g y]

solve size solution n xy = do
    guard (valid size solution xy)
    let solution'   = M.insert xy n solution
        sortedMoves = Task1.sortOn (neighbors size solution) (sequence moves xy)
    if n == size * size
        then [solution']
        else sortedMoves >>= solve size solution' (n+1)

printBoard size solution = board [0..size-1] where
    sqSize    = size * size
    elemSize  = length (show sqSize)
    separator = intercalate (replicate elemSize '-') (replicate (size + 1) "-")
    pad n s   = replicate (elemSize - length s) ' ' ++ s
    elem xy   = pad elemSize . show $ solution M.! xy
    line y    = concat  . intersperseWrap "|" $ [elem (x, y) | x <- [0..size-1]]
    board     = unlines . intersperseWrap separator . map line
    intersperseWrap s ss = s : intersperse s ss ++ [s]

go size xy = case solve size M.empty 1 xy of
    []    -> "No solution found"
    (s:_) -> printBoard size s

task1 xy = do
    args <- getArgs
    name <- getProgName
    putStrLn $ case map reads args of
        []             -> go 8 xy
        [[(size, "")]] -> go size xy
        _              -> "Usage: " ++ name ++ " <size>"
