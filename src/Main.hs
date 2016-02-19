{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import Data.Maybe

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main =
    T.interact $
    newLine . T.decodeUtf8 . encode . sumObjects . map parse . T.lines
  where
    parse = fromMaybe (error "parse failed") . decode . T.encodeUtf8
    newLine = T.unlines . (: [])

sumObjects :: [Value] -> Value
sumObjects = Object . foldl' go HM.empty
  where
    go acc (Object o) = HM.unionWith f acc o
    go _ _ = error "expected Object"
    f (Number new) (Number old) = Number (new + old)
    f (Object new) (Object old) = Object $ go old (Object new)
    f new Null = new
    f _ _ = error "expected Number or Object."
