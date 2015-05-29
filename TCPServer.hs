import Network.Socket
 
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
    send sock "Hi!\n"
    sClose sock