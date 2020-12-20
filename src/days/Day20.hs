module Day20 where

import Utils ( groupLines )
import qualified Data.Map as M

type Edge = [Bool]
type Tile = (Int, [Edge]) -- Top, Right, Bottom, Left

day20part1 :: IO ()
day20part1 = do
    input <- groupLines <$> readFile "input/day20input.txt"
    let tiles = map getTile input
    let tileConnections = [(fst t, length $ filter (areConnected t) (filter (/= t) tiles)) | t <- tiles]
    print $ product [fst t | t <- filter ((==2). snd) tileConnections]

getTile :: [String] -> Tile
getTile ss = 
    let 
        id = (read . init . last . words . head) ss 
        edges = [getEdge (ss!!1), getEdge [last s | s <- tail ss], getEdge (last ss), getEdge [head s | s <- tail ss]]
    in
        (id, edges) 

getEdge :: String -> Edge
getEdge s = [c == '#' | c <- s]

areConnected :: Tile -> Tile -> Bool 
areConnected t1 t2 = or [e1 == e2 || reverse e1 == e2 || reverse e2 == e1| e1 <- snd t1 , e2 <- snd t2]

day20part2 :: IO ()
day20part2 = print "TODO"