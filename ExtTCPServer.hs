import Network.Socket
import System.Random

-- min and max both inclusive
rand :: Int -> Int -> IO Int
rand min max = getStdRandom $ randomR (min,max)

main :: IO ()
main = do
    -- create socket
    sock <- socket AF_INET Stream 0
    -- make socket immediately reusable - eases debugging.
    setSocketOption sock ReuseAddr 1
    -- listen on TCP port 55555
    bindSocket sock (SockAddrInet 55555 iNADDR_ANY)
    -- allow a maximum of 2 outstanding connections
    listen sock 2
    mainLoop sock
 
mainLoop :: Socket -> IO ()
mainLoop sock = do
    -- accept one connection and handle it
    conn <- accept sock
    runConn conn
    mainLoop sock
 
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, addr) = do
    putStrLn $ "Connected client: " ++ show addr
    contents <- readFile "data.txt"
    n <- rand 0 6
    
    let line = lines contents !! n
    
    send sock line
    sClose sock