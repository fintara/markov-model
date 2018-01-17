module MarkovModel 
    ( createKGrams
    , frequency
    , probability
    , generate
    ) where


import qualified Data.List  as L
import qualified Data.Map   as M
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text  as T
import           System.Random (randomRIO)


type KGram  = Text
type Next   = Text
type Link   = (KGram, Next)
type KGrams = [Link]

type KGramTable a     = M.Map Link a
type FrequencyTable   = KGramTable Int
type ProbabilityTable = KGramTable Double


createKGrams :: Int -> Text -> KGrams
createKGrams k text = map pair [0..T.length text - 1]
    where 
        circular = text `T.append` (T.take k text)
        pair     = (,) <$> gram <*> next
        gram x   = T.take k . T.drop x     $ circular
        next x   = T.take 1 . T.drop (x+k) $ circular


findKGram :: KGram -> KGramTable a -> KGramTable a
findKGram t = M.fromList . filter (\(k,_) -> fst k == t) . M.toList


frequency :: KGrams -> FrequencyTable
frequency = foldr count M.empty
        where count k = M.insertWith (+) k 1


probability :: FrequencyTable -> ProbabilityTable
probability m = M.fromList . map toProb . M.toList $ m
          where 
            toProb :: (Link, Int) -> (Link, Double)
            toProb (link, cnt) = (link, (fromIntegral cnt) / fromIntegral (total $ findKGram (fst link) m))

            total :: FrequencyTable -> Int
            total = sum . map snd . M.toList


next :: KGram -> ProbabilityTable -> IO Next
next gram table = do
    r <- randomRIO (0.0, 1.0)
    let csum = scanl1 (+) . map snd $ list
        idx  = fromJust . L.findIndex (\x -> x >= r) $ csum
    return . snd.fst $ list !! idx
    where list = M.toList (findKGram gram table)


generate :: Int -> KGram -> ProbabilityTable -> IO Text
generate len seed table = generate' (T.empty, seed) [seed]
    where
        generate' :: (Next, KGram) -> [Text] -> IO Text
        generate' (c, gram) acc 
            | length acc > len = return . T.concat $ acc
            | otherwise        = do
                c' <- next gram table
                let gram' = T.tail gram `T.append` c'
                    acc'  = acc ++ [c']
                generate' (c', gram') acc'