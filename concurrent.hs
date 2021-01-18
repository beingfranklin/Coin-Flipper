import Control.Concurrent
import System.Random

data Coin = Head | Tail deriving (Show,Eq)

coinFlip :: IO Coin
coinFlip = do
    r <- randomIO :: IO Bool
    return $ if r then Head else Tail

process :: String -> MVar String -> MVar Coin -> IO()
process name winner box = do
    c1 <- takeMVar box
    putStrLn $ name ++ "'s turn"
    c2 <- coinFlip
    putStrLn $ " -- got " ++ (show c2)
    if c1 == c2 then
        putMVar winner $ "Process " ++ name ++ " wins!"
    else do
        putStrLn "-- putting coin back in the box"
        putMVar box c1
        threadDelay 5
        process name winner box

main :: IO ()
main = do
    coin <- coinFlip
    putStrLn $ "Random coin is:" ++ (show coin)
    box <- newMVar coin
    winner <- newEmptyMVar
    forkIO (process "A" winner box)
    forkIO (process "B" winner box)
    forkIO (process "C" winner box)
    w<- takeMVar winner
    putStrLn $ w

