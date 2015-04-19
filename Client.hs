import System.Environment
import System.Exit
import System.IO.Error (catchIOError)
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import qualified Data.Conduit as C

main :: IO ()
main = getArgs >>= parse

parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse (addr:[]) = startMonitor addr
parse _ = usage >> exit

usage   = putStrLn "Usage: hsync [-vh] [remote addr]"
version = putStrLn "hsync 0.1"
exit    = exitWith ExitSuccess

startMonitor = \addr -> do
    initReq <- parseUrl addr
    let req = initReq { method = "POST" }
    withManager $ \manager -> do
        liftIO $ catchIOError $ http req manager $ do
            putStrLn "Address is down..."
            exit




