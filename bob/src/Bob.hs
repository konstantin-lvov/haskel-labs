{-# LANGUAGE MultiWayIf #-}

module Bob (responseFor) where
import Data.Char

isUpperCase :: String -> Bool
isUpperCase (s:xs) = if | not (isAlpha s) -> isUpperCase xs
                        | isUpper s -> isUpperCase xs
                        | otherwise -> False
isUpperCase _ = True

hasQuestion :: String -> Bool
hasQuestion (s:xs) = if | isAlpha s -> hasQuestion xs
                        | s == '?'  -> True
                        | otherwise -> False
hasQuestion _ = False

responseFor :: String -> String
responseFor s = if | hasQuestion s -> "Точно"
                   | isUpperCase s -> "Эй, расслабься!"
                   | hasQuestion s && isUpperCase s -> "Остынь. Я знаю, что делаю!"
                   | s == "" -> "Бред какой-то"
                   | otherwise -> "Побоку"
