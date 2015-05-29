import Network.HTTP

get :: String -> String -> IO String
get host port = do
    let url = "http://" ++ host ++ ":" ++ port ++ "/"
    res <- simpleHTTP (getRequest url)
    body <- getResponseBody res
    return body

main = do
    body <- get "127.0.0.1" "55555"
    putStrLn body