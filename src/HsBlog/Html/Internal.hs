module HsBlog.Html.Internal where

import Numeric.Natural

newtype Html
    = Html String

newtype Structure
    = Structure String

newtype Content
    = Content String

newtype Head = Head String

type Title =
    String

html_ :: Head -> Structure -> Html
html_ (Head doc_head) (Structure content) =
    Html
        ( el
            "html"
            ( el "head" doc_head
                <> el "body" content
            )
        )

-- * head
title_ :: String -> Head
title_ = Head . el "title" . escape

stylesheet_ :: FilePath -> Head
stylesheet_ path =
    Head $ "<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> escape path <> "\">"

meta_ :: String -> String -> Head
meta_ name content =
    Head $ "<meta name=\"" <> escape name <> "\" content=\"" <> escape content <> "\">"

instance Semigroup Head where
    (<>) (Head h1) (Head h2) =
        Head (h1 <> h2)

instance Monoid Head where
    mempty = Head ""

-- * structures

empty_ :: Structure
empty_ = Structure ""

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

h_ :: Natural -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . getContentString

ul_ :: [Structure] -> Structure
ul_ =
    Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ =
    Structure . el "ol" . concatMap (el "li" . getStructureString)

pre_ :: String -> Structure
pre_ = Structure . el "pre" . escape

instance Semigroup Structure where
    (<>) c1 c2 =
        Structure (getStructureString c1 <> getStructureString c2)

instance Monoid Structure where
    mempty = empty_

-- * content

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path content =
    Content $
        elAttr
            "a"
            ("href=\"" <> escape path <> "\"")
            (getContentString content)

img_ :: FilePath -> Content
img_ path =
    Content $ "<img src=\"" <> escape path <> "\">"

b_ :: Content -> Content
b_ content =
    Content $ el "b" (getContentString content)

i_ :: Content -> Content
i_ content =
    Content $ el "i" (getContentString content)

instance Semigroup Content where
    (<>) c1 c2 =
        Content (getContentString c1 <> getContentString c2)

instance Monoid Content where
    mempty = Content ""

-- * utils

el :: String -> String -> String
el tag content =
    "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttr :: String -> String -> String -> String
elAttr tag attrs content =
    "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

render :: Html -> String
render html =
    case html of
        Html str -> str

getStructureString :: Structure -> String
getStructureString (Structure str) = str

getContentString :: Content -> String
getContentString (Content str) = str

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
        concatMap escapeChar
