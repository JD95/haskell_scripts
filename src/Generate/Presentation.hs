{-# LANGUAGE OverloadedStrings #-}
module Generate.Presentation (
  makePresHTML,
  makePresPDF,
  makePresLatex
) where

import Lucid
import Lucid.Html5
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Monoid
import Control.Monad
import Control.Arrow
import qualified Text.Pandoc as Pan
import Text.Pandoc.Options (def)
import Text.Pandoc.PDF (makePDF)
import Data.List
import Data.ByteString.Lazy.Internal

styles :: Html ()
styles = style_ ("body {"
  <> "margin: 2cm;"
  <> "font-family: 'Segoe UI', Arial, freesans, sans-serif;"
  <> "font-size: 300%;"
  <> "color: #333;"
  <> "}"
  <> "h1 {"
  <> "border-bottom: solid 1px lightgray;"
  <> "}"
  <> ".slide {"
  <> "page-break-after: always;"
  <> "}"
  )

slide :: T.Text -> Html () -> Html ()
slide t content =
  div_ [class_ "slide"] $ do
    h2_ [] (toHtml t)
    content

points :: [T.Text] -> Html ()
points = ul_ [] . mapM_ (li_ [] . toHtml)


makePresHTML :: [(T.Text, [T.Text])] -> String
makePresHTML = LT.unpack . renderText
             . doctypehtml_
             . (header_ styles >>)
             . body_ [] . mapM_ (second points >>> uncurry slide)

convertHtmlToLatex = either (const "Could not read file") (Pan.writeLaTeX def)
                   . Pan.readHtml def

makePresLatex = convertHtmlToLatex . makePresHTML

convertHtmlToPDF:: String -> IO (Either ByteString ByteString)
convertHtmlToPDF = either (const (pure . Left $ "Could not read file")) (makePDF "pdflatex" Pan.writeLaTeX def)
                 . Pan.readHtml def

makePresPDF :: [(T.Text, [T.Text])] -> IO ByteString
makePresPDF = fmap (either id id) . convertHtmlToPDF . makePresHTML
