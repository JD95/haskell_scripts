{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Generate.Presentation
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as BL
import Lucid

someFunc :: IO ()
someFunc = do
  let pres = [("Title 1" , []), ("Title 2", ["a","b","c"])]
  --BL.writeFile "pres.pdf" pres
  writeFile "pres.tex" (makePresLatex pres)
--  writeFile "pres.html" (T.pack makePresHTML pres)
