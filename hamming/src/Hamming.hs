{-# LANGUAGE MultiWayIf #-}
module Hamming (distance) where

distanceIs :: String -> String -> Int
distanceIs (x:xs) (y:ys) = if | x /= y    -> 1 + (distanceIs xs ys)
                              | otherwise -> 0 + (distanceIs xs ys)
distanceIs [] [] = 0

distance :: String -> String -> Maybe Int
distance xs ys = if | (length xs) /= (length ys) -> Nothing
                    | otherwise -> Just (distanceIs xs ys)
