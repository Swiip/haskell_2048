module Lib
    ( someFunc
    , initBoard
    , flatGrid
    , emptyCells
    , chooseRandomTile
    , grid
    , currentId
    , reducer
    , State(..)
    , Action(..)
    , ActionType(..)
    ) where

data Tile = Tile { i :: Int
                 , value :: Int
                 , row :: Int
                 , column :: Int
                 } deriving (Show)

data Board = Board { grid :: [[Tile]]
                   , currentId :: Int
                   } deriving (Show)

data State = State { board :: Board
                   , changed :: Bool
                   , won :: Bool
                   , lost :: Bool
                   } deriving (Show)

data ActionType = Init | Move deriving (Show, Eq)

data Action = Action { actionType :: ActionType
                     , direction :: Int
                     , randomValue :: Float
                     , randomPosition :: Float
                     }  deriving (Show)

newTile :: Int -> Tile
newTile i = Tile {i=i, value=0, row=0, column=0}

dimension :: Int
dimension = 3

fourProbability :: Float
fourProbability = 0.2

range :: [Int]
range = [0..dimension]

initRowReducer :: (Int, [Tile]) -> a -> (Int, [Tile])
initRowReducer (i, row) _ = (i+1, (newTile i):row)

initRow :: Int -> (Int, [Tile])
initRow i = foldl initRowReducer (i, []) range

initGridReducer :: (Int, [[Tile]]) -> a -> (Int, [[Tile]])
initGridReducer (i, grid) _ = let (i', row) = initRow i
                              in (i', row:grid)

initGrid :: Int -> (Int, [[Tile]])
initGrid i = foldl initGridReducer (i, []) range

initBoard :: Board
initBoard = let (i, grid) = initGrid 0
            in Board { grid=grid, currentId=i }

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

flatGrid :: Board -> [(Int, Int, Tile)]
flatGrid board = let rows = enumerate . grid
                     columns (x, ts) = map (\(y, t) -> (x, y, t)) (enumerate ts)
                 in  concat (map columns (rows board))

emptyCells :: Board -> [(Int, Int, Tile)]
emptyCells board = filter (\(_, _, t) -> value t == 0) (flatGrid board)

chooseRandomTile :: Board -> Float -> Float -> (Int, Int, Int)
chooseRandomTile board randomPosition randomValue =
  let cells = emptyCells board
      index = floor (randomPosition * (fromIntegral (length cells)))
      (x, y, _) = cells!!index
      value = if randomValue < fourProbability then 4 else 2
  in (x, y, value)

addTile :: Board -> (Int, Int, Int) -> Board
addTile board (x, y, v) = let myGrid = grid board
                              myId = currentId board
                              newTile = Tile {i=myId, value=v, row=0, column=0}
                              rowMapper (i, tile) = if i == y then newTile else tile
                              addTileRow row = map rowMapper (enumerate row)
                              columnMapper (i, row) = if i == x then addTileRow row else row
                              addTileColumn = map columnMapper (enumerate myGrid)
                          in Board { grid=addTileColumn, currentId=myId + 1 }

update :: Board -> Board
update board = let myGrid = grid board
                   myId = currentId board
                   rowMapper x (y, tile) = Tile {i=i tile, value=value tile, row=x, column=y}
                   columnMapper (x, row) = map (rowMapper x) (enumerate row)
                   updateColumn = map columnMapper (enumerate myGrid)
               in Board { grid=updateColumn, currentId=myId }

initAction :: Action -> State
initAction action = let boardEmpty = initBoard
                        newTile = chooseRandomTile boardEmpty (randomPosition action) (randomValue action)
                        boardWithFirstTile = addTile boardEmpty newTile
                        board = update boardWithFirstTile
                    in State { board=board, changed=False, won=False, lost=False }

reducer :: State -> Action -> State
reducer state action
  | actionType action == Init = initAction action
  | otherwise = state

someFunc :: IO ()
someFunc = putStrLn (show (chooseRandomTile initBoard 0.3 0.4))
