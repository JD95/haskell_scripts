{-# LANGUAGE OverloadedStrings #-}
module Generate.Presentation ( makePresHTML
                             , makePresPDF
                             , makePresLatex
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
import Clay hiding (id)

import qualified Data.ByteString.Char8 as B
import qualified Lookup.Reddit as Reddit

type Pres = [(T.Text, [T.Text])]

styles :: Html ()
styles = style_ . LT.toStrict . render $ do
  body ? do
    margin (cm 2) (cm 2) (cm 2) (cm 2)
    fontFamily ["Segoe UI", "Arial"] [sansSerif]
    fontSize (pct 100)
    color black
  h1 ? borderBottom solid (px 1) lightgray
  element ".slide" ? ("page-break-after" -: "always")

slide :: T.Text -> Html () -> Html ()
slide t content =
  div_ [class_ "slide"] $ do
    h2_ [] (toHtml t)
    content

points :: [T.Text] -> Html ()
points = ul_ [] . mapM_ (li_ [] . toHtml)

makePresHTML :: Pres -> String
makePresHTML = LT.unpack . renderText
             . doctypehtml_
             . (header_ styles >>)
             . body_ [] . mapM_ (second points >>> uncurry slide)

convertHtmlToLatex = either (const "Could not read file") (Pan.writeLaTeX def)
                   . Pan.readHtml def

makePresLatex = convertHtmlToLatex . makePresHTML

convertHtmlToPDF :: String -> IO (Either ByteString ByteString)
convertHtmlToPDF = either (const (pure . Left $ "Could not read file")) (makePDF "pdflatex" Pan.writeLaTeX def)
                 . Pan.readHtml def

makePresPDF :: Pres -> IO ByteString
makePresPDF = fmap (either id id) . convertHtmlToPDF . makePresHTML

genPresFromReddit :: Monoid a => (Pres -> a) -> T.Text -> IO a
genPresFromReddit presF = Reddit.titlesFromThisWeek                  
                      >=> pure . either mempty (presF . fmap (flip (,) []) . take 10)

line :: T.Text -> T.Text
line l = l <> "\n"

bullet :: T.Text -> T.Text
bullet l = "\t" <> l <> "\n"

(><) :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
f >< g = first f . second g

infixr 8 ><

makePPTOutline :: Pres -> T.Text
makePPTOutline = T.concat . fmap (T.concat . uncurry (:) . (line >< (fmap bullet)))

redditPresentation subName fileName f =
    (B.pack . T.unpack) <$> (genPresFromReddit f subName) >>= B.writeFile fileName
