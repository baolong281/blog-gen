module HsBlog.Utils where

confirm :: String -> IO Bool
confirm message = do
    putStrLn message
    putStrLn "Are you sure? (y/n)"
    answer <- getLine
    case answer of
        "y" -> pure True
        "n" -> do
            pure False
        _ ->
            putStrLn "Invalid response. use y or n"
                *> confirm message
