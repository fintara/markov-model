{-# LANGUAGE OverloadedStrings #-}

module Main where

import           MarkovModel
import           Prelude hiding (readFile, putStrLn)
import           Data.Text.IO (readFile, putStrLn)
import qualified Data.Text as T
import           System.Environment (getArgs)


app :: Int -> Int -> T.Text -> IO T.Text
app k t text = 
    let table = probability . frequency . createKGrams k $ text
    in generate t (T.take k text) table


main :: IO ()
main = do
    args <- getArgs
    if length args /= 3
        then putStrLn "Usage: markov [k-gram-length] [T-result-length] [filename]"
        else putStrLn =<< app (read $ args !! 0) (read $ args !! 1) =<< (readFile $ args !! 2)