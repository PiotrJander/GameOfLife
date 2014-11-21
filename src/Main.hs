import Grid
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort(ViewPort)
import Data.Matrix
import Data.List
import Text.Printf
import System.Environment
import System.Directory

type Time  = Float
type Coors = (Float, Float)

main = do
	args <- getArgs
	if head args == "options"
		then available
		else life args

life :: [String] -> IO ()
life args = do
	let
		stepsPerSec = read (head args) :: Int
		patternName = last args
		fileName = printf "../patterns/%s.rle" patternName
	pattern <- fromFile fileName
	let d = InWindow "Game of Life" (1000, 1000) (10, 10)
	simulate d black stepsPerSec pattern render tick'

available :: IO ()
available = do
	files <- getDirectoryContents "../patterns"
	let
		beforeDot = fst . break (== '.')
		patterns = map beforeDot . filter (".rle" `isSuffixOf`) $ files
	mapM_ putStrLn patterns

render :: Grid -> Picture
render g = translate (-400) (-400) . scale zoom zoom . pictures . map f . findSquares $ g
	where
		n = fromIntegral $ ncols g
		zoom = 600/n
		f (a,b) = chaoticColor a b $ polygon [(b,a), (b+1, a), (b+1, a+1), (b, a+1)]
		stockColors = [makeColor8 255 253 153 200, makeColor8 75 204 0 200, makeColor8 74 153 28 200,
						makeColor8 96 65 255 200, makeColor8 31 48 204 200]
		chaoticColor i j = color (stockColors !! chaos)
			where chaos = (floor (i * j)) `mod` length stockColors

findSquares :: Grid -> [Coors]
findSquares g = [(fromIntegral x, fromIntegral y)|x <-[1..(nrows g)], y <-[1..(ncols g)], g ! (x,y) == 1]

tick' :: ViewPort -> Float -> Grid -> Grid
tick' _ _  = tick
