module Html.Internal where

import Numeric.Natural

newtype Html
    = Html String

newtype Structure
    = Structure String

instance Semigroup Structure where
    (<>) c1 c2 =
        Structure (getStructureString c1 <> getStructureString c2)

instance Monoid Structure where
    mempty = empty_

type Title =
    String

html_ :: Title -> Structure -> Html
html_ title (Structure content) =
    Html
        ( el
            "html"
            ( el "head" (el "title" (escape title))
                <> el "body" content
            )
        )

empty_ :: Structure
empty_ = Structure ""

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ =
    Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ =
    Structure . el "ol" . concatMap (el "li" . getStructureString)

pre_ :: String -> Structure
pre_ = Structure . el "pre" . escape

el :: String -> String -> String
el tag content =
    "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

render :: Html -> String
render html =
    case html of
        Html str -> str

getStructureString :: Structure -> String
getStructureString (Structure str) = str

escape :: String -> String
escape =
    let
        escapeChar c =
            case c of
                '<' -> "&lt;"
                '>' -> "&gt;"
                '&' -> "&amp;"
                '"' -> "&quot;"
                '\'' -> "&#39;"
                _ -> [c]
     in
        concat . map escapeChar
