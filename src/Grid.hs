module Grid
( Grid
, tick
, nthStep
, fromFile
, fromRLE
) where

import Data.List
import Data.Matrix
import Text.Regex.Posix
import System.IO

type Cell = Int  -- in fact only 0 and 1 are permitted
type Grid = Matrix Cell

tick :: Grid -> Grid
tick old = matrix m n $ newState old
	where
		m = nrows old
		n = ncols old

newState :: Grid -> (Int, Int) -> Cell
newState old (i,j)
	| old ! (i,j) == 0 && countAlive == 3         = 1
	| old ! (i,j) == 1 && countAlive `elem` [2,3] = 1
	| otherwise                                   = 0
	where
		neighboursCoors = [(i-1, j-1), (i, j-1), (i+1, j-1), (i-1, j),
						(i+1, j), (i-1, j+1), (i, j+1), (i+1, j+1)]
		neighboursValues = map (getElem' old) neighboursCoors
		countAlive = sum [ 1 | (Just 1) <- neighboursValues ]

getElem' :: Grid -> (Int, Int) -> Maybe Cell
getElem' g (i,j)
	| i > 0 && j > 0 && i <= m && j <= n = Just $ getElem i j g
	| otherwise = Nothing
	where
		m = nrows g
		n = ncols g

nthStep :: Grid -> Int -> Grid
nthStep g n = iterate tick g !! n

-- conversion from RLE

type RLE = String
type RLELines = [String]
type RLEData = String

fromFile :: FilePath -> IO Grid
fromFile filepath = do
	rle <- readFile filepath
	return $ fromRLE rle

fromRLE :: RLE -> Grid
fromRLE rle =
	let
		metadata = head $ raw rle
		n = getN metadata
		rows = makeRows n . getRowsData $ raw rle
	in
		fromLists rows

raw :: RLE -> RLELines
raw = filter (not . isComment) . lines

isComment :: String -> Bool
isComment str = str =~ "^[[:space:]]*$" || "#C " `isPrefixOf` str

getN :: String -> Int
getN str =
	let
		groups = str =~ "x[[:space:]]*=[[:space:]]*([0-9]+)" :: [[String]]
		n = if null groups
			then error "Invalid `x=' line."
			else read (last . head $ groups) :: Int
	in
		n

getRowsData :: RLELines -> RLEData
getRowsData = foldr (++) "" . tail

makeRows :: Int -> RLEData -> [[Cell]]
makeRows n "" = []
makeRows n s = addDead n row : makeRows n rest
	where (row, rest) = makeRow s

makeRow :: RLEData -> ([Cell], String)
makeRow ""  = ([], "")
makeRow "!" = ([], "")
makeRow ('b' : s) = (0 : fst (makeRow s), snd (makeRow s))
makeRow ('o' : s) = (1 : fst (makeRow s), snd (makeRow s))
makeRow ('$' : s) = ([], s)
makeRow s =
	let
		pat = "([0123456789]+)([bo$])"
		(_, _, rest, groups) = s =~ pat :: (String, String, String, [String])
		n = read (head groups) :: Int
		c = head $ last groups  -- get one of bo$ as a character
		expanded = replicate n c ++ rest
	in
		makeRow expanded
 
addDead :: Int -> [Cell] -> [Cell]
addDead n row = let missing = n - length row in row ++ replicate missing 0
