module Everything where

import Pirma
import Control.Lens
import Network.Wreq
import Network.HTTP.Types.Header
import Data.Aeson
import System.Random
import qualified Data.ByteString.Char8 as BytCh
import qualified Data.ByteString.Lazy.Char8 as BytStr
import qualified System.IO as SysIO
import qualified Data.CaseInsensitive as CI
import Data.List

type MovesList = [(Int, Int)]
type Move = (Int, Int)

main :: IO()
main = do
  getMoves



--Requests
url :: String
url = "http://tictactoe.homedir.eu/game/tp_mj9/player/2"

headerNamePost :: HeaderName
headerNamePost = CI.mk (BytCh.pack "Content-Type")

headerNameGet :: HeaderName
headerNameGet = CI.mk (BytCh.pack "Accept")

postMoves :: String -> IO ()
postMoves msg = do
    let opts = defaults & header headerNamePost  .~ [BytCh.pack "application/bencode+map"]
    newMove <- getRandomMove msg
    r <- postWith opts url $ BytCh.pack (addMove newMove msg)
    let isWinner = winner (addMove newMove msg)
    if (isWinner == Just O) then SysIO.putStrLn ("Laimejote")
        else getMoves

getMoves :: IO ()
getMoves = do
    let opts = defaults & header headerNameGet .~ [BytCh.pack "application/bencode+map"]
    r <- getWith opts url
    let isWinner = winner (BytStr.unpack (r ^. responseBody))
    if (isWinner == Just X) then SysIO.putStrLn ("Pralosete") 
        else if (length (decodeMovesMade (BytStr.unpack (r ^. responseBody))) == 9) then SysIO.putStrLn ("Lygiosios") 
            else postMoves (BytStr.unpack (r ^. responseBody))

--Getting Random Move
addMove :: Move -> String -> String
addMove (x, y) msg = let
                        msgCut = take (length msg - 1) msg
                        turnsMade = length (decodeMovesMade msg)
                        turnString = intToString turnsMade
                        xString = intToString x
                        yString = intToString y
                    in msgCut ++ "1:" ++ turnString ++ "d1:v1:o1:xi" ++ xString ++ "e1:yi" ++ yString ++ "eee"

intToString :: Int -> String
intToString a = show a

moves :: MovesList
moves = [(2, 2), (1, 1), (0, 2), (1, 2), (1, 0), (0, 1), (2, 1), (2, 0), (0, 0)]

getMove :: Int -> [(Int, Int)] -> (Int, Int)
getMove b a = a !! b

decodeMovesMade :: String -> MovesList
decodeMovesMade movesMade = let
                                movesToExternalMap = decodeFullMsg movesMade
                            in getMovesList movesToExternalMap []

getMovesList :: ExternalMap -> MovesList -> MovesList
getMovesList [] movesList = movesList
getMovesList externalMap movesList = let
                                        (singleInternal, restExternal) = popInternalMap externalMap
                                        x = checkValue singleInternal "x"
                                        xInt = parseInt x
                                        y = checkValue singleInternal "y"
                                        yInt = parseInt y
                                    in getMovesList restExternal ((xInt, yInt) : movesList)

filterMoves :: MovesList -> MovesList-> MovesList
filterMoves [] leftMoves = leftMoves
filterMoves movesList leftMoves = let
                                    (singleMove, restMoves) = popMoveFromMovesList movesList
                                    leftMoves' =  filter (\n -> n /= singleMove) leftMoves
                                in filterMoves restMoves leftMoves'


popMoveFromMovesList :: MovesList -> (Move, MovesList)
popMoveFromMovesList movesList = let
                                    (a : b) = movesList
                                in (a, b)


getRandomMove :: String -> IO Move
getRandomMove movesMadeString = let
                                  movesMade = decodeMovesMade movesMadeString
                                  filteredMoves = filterMoves movesMade moves
                                  randomMove = pick filteredMoves
                                in randomMove

pick :: [a] -> IO a
pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)