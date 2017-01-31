module Generate.GoogleSlidesTypes ( Page(..)
                                  , PageElement(..)
                                  , Size(..)
                                  , Dimension(..)
                                  , Unit(..)
                                  , PageProperties(..)
                                  , PageBackgroundFill(..)
                                  , PropertyState(..)
                                  , SolidFill(..)
                                  , OpaqueColor(..)
                                  , RgbColor(..)
                                  , ColorScheme(..)
                                  , ThemeColorPair(..)
                                  , ThemeColorType(..)
                                  , SlideProperties(..)
                                  , AffineTransform(..)
                                  ) where

data PageType = SLIDE -- ^ A slide page.
              | MASTER -- ^ A master slide page.
              | LAYOUT -- ^ A layout page.

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
                 , size :: Size
                 , transform :: AffineTransform
                 , title :: String
                 , description :: String
                 }

data Size = Size
          { width :: Dimension
          , height :: Dimension
          }

data Dimension = Dimension
               { magnitude :: Double
               , dimensionUnit :: Unit
               }

data Unit = UNIT_UNSPECIFIED -- ^ The units are unknown.
          | EMU -- ^ 	An English Metric Unit (EMU) is defined as 1/360,000 of a centimeter and thus there are 914,400 EMUs per inch, and 12,700 EMUs per point.
          | PT -- ^ A point, 1/72 of an inch.

data PageProperties = PageProperties
                    { pageBackgroundFill :: PageBackgroundFill
                    , colorScheme :: ColorScheme
                    }

data PageBackgroundFill = PageBackgroundFill
                        { propertyState :: PropertyState
                        , solidFill :: SolidFill
                        }
                          
data PropertyState = RENDERED
                   | NOT_RENDERED
                   | INHERIT

data SolidFill = SolidFill
               { color :: OpaqueColor
               , alpha :: Double
               }
                 
data OpaqueColor = OpqaueColor
                 { rgbColor :: RgbColor
                 }

data RgbColor = RbgColor
              { red :: Double
              , green :: Double
              , blue :: Double
              }



data ColorScheme = ColorScheme
                 { colors :: [ThemeColorPair]
                 }

data ThemeColorPair = ThemeColorPair
                    { themeType :: ThemeColorType
                    , themeColor :: RgbColor
                    }

data ThemeColorType = THEME_COLOR_TYPE_UNSPECIFIED
                    | DARK1
                    | LIGHT1
                    | DARK2
                    | LIGHT2
                    | ACCENT1
                    | ACCENT2
                    | ACCENT3
                    | ACCENT4
                    | ACCENT5
                    | ACCENT6
                    | HYPERLINK
                    | FOLLOWED_HYPERLINK
                    | TEXT1
                    | BACKGROUND1
                    | TEXT2
                    | BACKGROUND2
                      

data SlideProperties = SlideProperties
                     { layoutObjectId :: String
                     , masterObjectId :: String
                     }

data AffineTransform = AffineTransform
                     { scaleX :: Double
                     , scaleY :: Double
                     , shearX :: Double
                     , shearY :: Double
                     , translateX :: Double
                     , translateY :: Double
                     , affineUnit :: Unit
                     }
