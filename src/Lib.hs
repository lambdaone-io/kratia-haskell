module Lib
    ( someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

otherFunc :: Int -> Int
otherFunc x = x + 1