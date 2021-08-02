greeting :: IO ()
greeting = do putStr "Tell me your name: "
              name <- getLine
              putStrLn ("Hello " ++ name ++ " !") 
