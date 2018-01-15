{-# LANGUAGE OverloadedStrings #-}

module MarkovModel where

import Data.Text (Text)
import qualified Data.Text as T

type KGram = Text
type Next  = Text

createKGrams :: Int -> Text -> [(KGram, Next)]
createKGrams k text = map pair [0..T.length text - k]
    where 
        circular = text `T.append` (T.take 1 text)
        pair     = (,) <$> gram <*> next
        gram x   = T.take k . T.drop x     $ circular
        next x   = T.take 1 . T.drop (x+k) $ circular