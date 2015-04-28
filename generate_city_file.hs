import Data.Char
import Data.List


data Point = Point Float Float
data Vertex = Vertex Int Point


readVertex :: Int -> String -> Vertex
readVertex name line = Vertex name location
    where [x, y] = map read $ words line
          location = Point x y

distance :: Point -> Point -> Float
distance (Point px py) (Point qx qy) = sqrt((px - qx)*(px - qx) + (py - qy)*(py - qy))

weight :: Vertex -> Vertex -> Float
weight (Vertex _ l1) (Vertex _ l2) = distance l1 l2

shortestPath :: [Vertex] -> [Float]
shortestPath vertices = map pathLength $ permutations vertices

pathLength :: [Vertex] -> Float
pathLength vertices = sum $ zipWith distance start end
    where start = [p | Vertex n p <- vertices]
          end = drop 1 $ cycle start

-- Read in a list of city locations from standard input (each line should have
-- two space-separated floating point numbers).  Print out the solution to the
-- traveling salesperson problem on standard input, assuming you start at the
-- city on the first line of standard input.  The solution is a series of
-- integers (cooresponding to the line number on standard input) that the
-- salesperson is passing through.
main = do
    contents <- getContents
    mapM_ print $ shortestPath $ zipWith readVertex [1..] (lines contents)
