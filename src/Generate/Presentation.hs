{-# LANGUAGE OverloadedStrings #-}
module Generate.Presentation ( 
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

import qualified Data.ByteString.Char8 as B
import qualified Lookup.Reddit as Reddit

type Pres = [(T.Text, [T.Text])]

data PageType = TODO

{- Matching specification from https://developers.google.com/slides/reference/rest/v1/presentations.pages#Page.PageElement -}

data Page = Page
          { pageId :: String
          , pageType :: PageType
          , pageElements :: [PageElement]
          , pageProperties :: PageProperties
          , slideProperties :: SlideProperties
          }

data PageElement = PageElement
                 { elementId :: String
                 , size :: Int
                 , transform :: AffineTransform
                 , title :: String
                 , description :: String
                 }

data Size = Size { width :: Dimension
                 , height :: Dimension
                 }

data Dimension = Dimension { magnitude :: Int
                           , unit :: Unit
                           }

data Unit = UNIT_UNSPECIFIED -- ^ The units are unknown.
          | EMU -- ^ 	An English Metric Unit (EMU) is defined as 1/360,000 of a centimeter and thus there are 914,400 EMUs per inch, and 12,700 EMUs per point.
          | PT -- ^ A point, 1/72 of an inch.

data PageProperties = T2

data SlideProperties = T3

data AffineTransform = T4

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
