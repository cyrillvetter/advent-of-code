import Data.List.Split (splitOneOf)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import qualified Data.Map as M

type Point = (Int, Int)
type Grid = M.Map Point Int

main = do
    input <- lines <$> readFile "inputs/3.txt"
    let squares = foldl setSquare M.empty $ concatMap (generateArea . parse) input 
    print $ M.size $ M.filter (> 1) squares

generateArea :: (Point, Point) -> [Point]
generateArea ((p1, p2), (wide, tall)) = [(x, y) | x <- [p1..p1+wide-1], y <- [p2..p2+tall-1]]

setSquare :: Grid -> Point -> Grid
setSquare map key = M.insertWith (+) key 1 map

parse :: String -> (Point, Point)
parse = toDoubleTuple . mapMaybe readMaybe . splitOneOf " :,x"

toDoubleTuple :: [a] -> ((a, a), (a, a))
toDoubleTuple [a, b, c, d] = ((a, b), (c, d))
