module Day20 where

import Data.List ( sortBy, transpose )
import Utils ( groupLines )
import qualified Data.Map as M

data Orientation = Unchanged | C90 | C180 | C270 deriving (Show, Eq)
type Positioning = (Orientation, Bool)

type TileBody = [String]
type Edge = [Bool]
type Tile = (Int, [Edge], TileBody) -- Top, Right, Bottom, Left

type Coord = (Int, Int)

type ConnectionsMap = M.Map Int [Int]
type TileMap = M.Map Int Tile
type PictureMap = M.Map Coord Tile

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

day20part1 :: IO ()
day20part1 = do
    input <- groupLines <$> readFile "input/day20input.txt"
    let tiles = map getTile input
    let tileConnections = [(fst3 t, length $ filter (areConnected t) (filter (/= t) tiles)) | t <- tiles]
    print $ product [fst t | t <- filter ((==2). snd) tileConnections]

getTile :: [String] -> Tile
getTile ss = 
    let 
        id = (read . init . last . words . head) ss 
        body = tail ss
        edges = [getTopEdge body, getRightEdge body, getBottomEdge body, getLeftEdge body]
    in
        (id, edges, body)

getEdge :: String -> Edge
getEdge s = [c == '#' | c <- s]

areConnected :: Tile -> Tile -> Bool 
areConnected t1 t2 = or [e1 == e2 || reverse e1 == e2 || reverse e2 == e1| e1 <- snd3 t1 , e2 <- snd3 t2]

getTopEdge :: TileBody -> Edge
getTopEdge = getEdge . head

getLeftEdge :: TileBody -> Edge
getLeftEdge = getEdge . reverse . map head

getRightEdge :: TileBody -> Edge
getRightEdge = getEdge . map last

getBottomEdge :: TileBody -> Edge
getBottomEdge = getEdge . reverse . last

day20part2 :: IO ()
day20part2 = do
    input <- groupLines <$> readFile "input/day20input.txt"
    let tiles = map getTile input
    let tileMap = M.fromList [(fst3 x, x) | x <- tiles]
    let edgeLength = head $ dropWhile ((/= length tiles) . (^2)) [0..]

    let tileConnections = [(fst3 t, map fst3 $ filter (areConnected t) (filter (/= t) tiles)) | t <- tiles]
    let connectionsMap = M.fromList tileConnections

    let topCorner = head $ filter ((==2). length. snd) tileConnections

    let topLeftTile = M.findWithDefault errorTile (fst topCorner) tileMap
    let tile10 = M.findWithDefault errorTile (head $ snd topCorner) tileMap
    let tile01 = M.findWithDefault errorTile (last $ snd topCorner) tileMap

    let coordsToFillIn = concat [[(x, y) | x <- [1..edgeLength-1], y <- [1..edgeLength-1], x + y == z] ++ if z < edgeLength then [(0, z), (z, 0)] else []| z <- [2..2*edgeLength]]
    let picMap = M.fromList [ ((0, 0), topLeftTile), ((1, 0), tile10), ((0, 1), tile01)]
    let finalMap = foldl (fillInCoord tileMap connectionsMap) picMap coordsToFillIn

    -- First figure out 0,0 orientation
    let topLeftOrientation = getStartingOrientation topLeftTile tile10 tile01

    let leftHandEdgePositionings = foldl (generateToBottom finalMap) [topLeftOrientation] [(0, x) | x <- [1..edgeLength-1]]
    let allPositionings = [foldl (generateToRight finalMap) [leftHandEdgePositionings !! z] [(x, z) | x <- [1..edgeLength-1]] | z <- [0..edgeLength - 1]]
    -- Indexed column row i.e. (allPositions !! column) !! row

    let theOneTrueTile = concat [getFinalTileRow y finalMap allPositionings | y <- [0 .. length allPositionings -1 ]]

    let seaMonsterCount = sum [getSeaMonsterCount $ manipulateTileBody theOneTrueTile pos | pos <- positions]

    print $ length ( filter (=='#') $ concat theOneTrueTile) - (seaMonsterCount * length seaMonsterLocs)


getSeaMonsterCount :: TileBody -> Int
getSeaMonsterCount t = length $ filter ((==True) . fst) [(getSeaMonster t (x, y), (x, y)) | x <- [0 .. length (head t) - 20], y <- [0 .. length t - 3]]

seaMonsterLocs :: [Coord]
seaMonsterLocs = [(18, 0), (0, 1), (5, 1), (6, 1), (11, 1), (12, 1), (17, 1), (18, 1), (19, 1), (1, 2), (4, 2), (7, 2), (10, 2), (13, 2), (16, 2)]

getSeaMonster :: TileBody -> Coord -> Bool 
getSeaMonster tile (x, y) = all (\(x', y') -> ((tile !! (y+y')) !! (x+x')) == '#') seaMonsterLocs

getFinalTileRow :: Int -> PictureMap -> [[Positioning]] -> TileBody
getFinalTileRow y picMap allPositionings = 
    let 
        allTiles = [getTileBodyOrientedAndTrimmed (M.findWithDefault errorTile c picMap) ((allPositionings !! snd c) !! fst c) | c <- [(x, y) | x <- [0.. length allPositionings - 1]]]
    in 
        [concat [(allTiles !! numTile) !! row | numTile <- [0.. length allTiles - 1]] | row <- [0 ..length (head allTiles) - 1]]

generateToRight :: PictureMap -> [Positioning] -> Coord -> [Positioning]
generateToRight picMap poss coord = 
    let 
        tileBodyAtHead = getTileBodyOriented (M.findWithDefault errorTile (fst coord - 1, snd coord) picMap) (last poss)
        tileToRight = M.findWithDefault errorTile coord picMap
        pos = getPosToRight tileBodyAtHead tileToRight
    in 
        poss ++ [pos]


generateToBottom :: PictureMap -> [Positioning] -> Coord -> [Positioning]
generateToBottom picMap poss coord = 
    let 
        tileBodyAtHead = getTileBodyOriented (M.findWithDefault errorTile (fst coord, snd coord - 1) picMap) (last poss)
        tileToBottom = M.findWithDefault errorTile coord picMap
        pos = getPosToBottom tileBodyAtHead tileToBottom
    in 
        poss ++ [pos]


positions :: [Positioning]
positions = [(Unchanged, False), (C90, False), (C180, False), (C270, False), (Unchanged, True), (C90, True), (C180, True), (C270, True)]

getPosToRight :: TileBody -> Tile -> Positioning
getPosToRight body rightTile = head $ dropWhile ((/= getRightEdge body) . reverse . getLeftEdge . getTileBodyOriented rightTile) positions

getPosToBottom :: TileBody -> Tile -> Positioning
getPosToBottom body bottomTile = head $ dropWhile ((/= getBottomEdge body) . reverse . getTopEdge . getTileBodyOriented bottomTile) positions


getTileBodyOrientedAndTrimmed :: Tile -> Positioning -> TileBody
getTileBodyOrientedAndTrimmed t pos = map (tail . init) $ init $ tail $ getTileBodyOriented t pos

getTileBodyOriented :: Tile -> Positioning -> TileBody
getTileBodyOriented (_, _, ss) = manipulateTileBody ss

manipulateTileBody :: TileBody -> Positioning -> TileBody
manipulateTileBody ss (or, flipped) = getRotated or (if flipped then map reverse ss else ss)

getRotated :: Orientation -> TileBody -> TileBody
getRotated or ss 
    | or == Unchanged = ss
    | or == C90 = rotCW ss
    | or == C180 = rotCW $ rotCW ss
    | or == C270 = rotCW $ rotCW $ rotCW ss
    

rotCW :: TileBody -> TileBody
rotCW = map reverse . transpose

errorTile :: Tile
errorTile = (-1, [], [])

getNumberOfTurns:: Orientation -> Int
getNumberOfTurns or 
    | or == Unchanged = 0
    | or == C90 = 1
    | or == C180 = 2
    | or == C270 = 3

getFlippedEdges :: [Edge] -> [Edge]
getFlippedEdges es = map reverse [head es, es!!3, es!!2, es!!1]

getRot :: Int -> Orientation
getRot x 
    | x == 0 = C90
    | x == 1 = Unchanged
    | x == 2 = C270
    | x == 3 = C180

getStartingOrientation :: Tile -> Tile -> Tile -> Positioning
getStartingOrientation tile tileToTheRight tileToTheBottom = 
    let 
        edges = snd3 tile
        edgesFlipped = getFlippedEdges $ snd3 tile
        rightEdges = snd3 tileToTheRight ++ getFlippedEdges (snd3 tileToTheRight)
        bottomEdges = snd3 tileToTheBottom ++ getFlippedEdges (snd3 tileToTheBottom)
        rotation = dropWhile (\x -> not ((edges !! (x `mod` 4)) `elem` rightEdges && (edges !! ((x+1) `mod` 4)) `elem` bottomEdges)) [0..3]
        rotationFlipped = dropWhile (\x -> not ((edgesFlipped !! (x `mod` 4)) `elem` rightEdges && (edgesFlipped !! ((x+1) `mod` 4)) `elem` bottomEdges)) [0..3]
    in 
        if not $ null rotation then
            (getRot $ head rotation, False)
        else
            (getRot $ head rotationFlipped, True)


fillInCoord :: TileMap -> ConnectionsMap -> PictureMap -> Coord -> PictureMap
fillInCoord tMap cMap picMap c = 
    let 
        isEdge = fst c == 0 || snd c == 0
        relevantTileCoords = if isEdge then [(max 0 (fst c - 1), max 0 (snd c - 1))] else [(fst c - 1, snd c), (fst c, snd c - 1)]
        relevantIds = [fst3 $ M.findWithDefault errorTile x picMap | x <- relevantTileCoords]
        couldBeConnected = [M.findWithDefault [] x cMap | x <- relevantIds]
        existingKeys = map fst3 (M.elems picMap)
        resTile = if isEdge then head [x | x <- head couldBeConnected, x `notElem` existingKeys] else head $ filter (`notElem` existingKeys) $ filter (`elem` (head couldBeConnected)) (last couldBeConnected)
    in
        M.insert c (M.findWithDefault errorTile resTile tMap) picMap
