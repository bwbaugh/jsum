{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T

main :: IO ()
main =
    T.putStrLn . T.decodeUtf8 . encode . sumObjects . map parse . T.lines
    =<< T.getContents
  where
    parse x = case eitherDecode' (T.encodeUtf8 x) of
        Right v -> v
        Left e -> error $ "parse failed: " ++ e

sumObjects :: [Value] -> Value
sumObjects = Object . foldl' go HM.empty
  where
    go acc (Object o) = HM.unionWith f acc o
    go acc Null = acc
    go _ _ = error "expected Object"
    f (Number new) (Number old) = Number (new + old)
    f (Object new) (Object old) = Object $ go old (Object new)
    f Null old = old
    f new Null = new
    f _ _ = error "expected Number or Object."
