import Grid
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort(ViewPort)
import Data.Matrix
import Data.List
import Text.Printf
import System.Environment
import System.Directory
import System.Random (randomRIO)

type Time  = Float
type Coors = (Float, Float)
type ColorStr = String

main = do
	args <- getArgs
	if head args == "patterns"
		then listPatterns
		else life args

life :: [String] -> IO ()
life args = do
	palettesFile <- readFile "../palette"
	-- gen <- getStdGen
	let
		stepsPerSec = read (head args) :: Int
		patternName = last args
		fileName = printf "../patterns/%s.rle" patternName
		palettes = map (map toColor) . map words . lines $ palettesFile
		bg = makeColor8 35 35 35 255
	colors <- pick palettes
	pattern <- fromFile fileName
	let d = InWindow "Game of Life" (1000, 1000) (10, 10)
	simulate d bg stepsPerSec pattern (render colors) tick'

toColor :: ColorStr -> (Picture -> Picture)
toColor = color . makeRGB . map parseHex . chunks
	where
		chunks [a,b,c,d,e,f] = [[a,b], [c,d], [e,f]]
		makeRGB [r,g,b] = makeColor8 r g b 200

pick :: [a] -> IO a
pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)

listPatterns :: IO ()
listPatterns = do
	files <- getDirectoryContents "../patterns"
	let
		beforeDot = fst . break (== '.')
		patterns = map beforeDot . filter (".rle" `isSuffixOf`) $ files
	mapM_ putStrLn patterns

render :: [Picture -> Picture] -> Grid -> Picture
render colors g = translate (-400) (-400) . scale zoom zoom . pictures . map f . findSquares $ g
	where
		n = fromIntegral $ ncols g
		zoom = 600/n
		f (a,b) = chaoticColor a b $ polygon [(b,a), (b+1, a), (b+1, a+1), (b, a+1)]
		-- stockColors = [makeColor8 255 253 153 200, makeColor8 75 204 0 200, makeColor8 74 153 28 200,
		-- 				makeColor8 96 65 255 200, makeColor8 31 48 204 200]
		chaoticColor i j = colors !! chaos
			where chaos = (floor (i * j)) `mod` length colors

findSquares :: Grid -> [Coors]
findSquares g = [(fromIntegral x, fromIntegral y)|x <-[1..(nrows g)], y <-[1..(ncols g)], g ! (x,y) == 1]

tick' :: ViewPort -> Float -> Grid -> Grid
tick' _ _  = tick

-- converting hex Strings to Int

hexChar :: Char -> Int
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'A' = 10
    | ch == 'B' = 11
    | ch == 'C' = 12
    | ch == 'D' = 13
    | ch == 'E' = 14
    | ch == 'F' = 15
    | otherwise     = 0

parseHex :: String -> Int
parseHex hxStr = go (reverse hxStr)
    where
    	go []     = 0
        go (x:xs) = hexChar x + 16 * parseHex xs