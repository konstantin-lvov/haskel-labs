{-# LANGUAGE MultiWayIf #-}

module DNA (toRNA) where

dNAtoRNA :: Char -> Char
dNAtoRNA 'G' = 'C'
dNAtoRNA 'C' = 'G'
dNAtoRNA 'T' = 'A'
dNAtoRNA 'A' = 'U'
dNAtoRNA  c  =  c

letterIsPartOfDNA :: Char -> Bool
letterIsPartOfDNA c = c `elem` "GCTA"

toRNA :: String -> Either Char String
toRNA (s:xs) = if | letterIsPartOfDNA s -> fmap (\x -> (dNAtoRNA s) : x) (toRNA xs)
                  | otherwise      -> Left s 
toRNA "" = Right ""
