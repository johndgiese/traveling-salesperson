import Data.Char
import Math


data Point = Point Float Float
data Vertex = Vertex { name :: String, location :: Point }


readVertex :: Int -> String -> Vertex
readVertex n line = Vertex {name=name, location=point}
    where name = show n
          coordinates = map (read :: Float) (words line)
          x = coordinates !! 0
          y = coordinates !! 1
          point = Point x y

distance :: Point -> Point -> Float
distance (Point px py) (Point qx qy) = Math.sqrt((px - qx)*(px - qx) + (py - qy)*(py - qy))

weight :: Vertex -> Vertex -> Float
weight v1 v2 = distance v1.location v2.location

shortestPath :: [Vertex] -> [String]
shortestPath _ = ["1", "2", "3"]  -- TODO: implement


main = do
    contents <- getContents
    print $ unlines path
    where
        vertices = zipWith readVertex [1..] (lines contents)
        path = shortestPath vertices
