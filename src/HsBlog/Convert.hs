module HsBlog.Convert where

import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
    case structure of
        Markup.Heading n txt ->
            Html.h_ n $ Html.txt_ txt
        Markup.Paragraph txt ->
            Html.p_ $ Html.txt_ txt
        Markup.UnorderedList list ->
            Html.ul_ $ map (Html.p_ . Html.txt_) list
        Markup.OrderedList list ->
            Html.ol_ $ map (Html.p_ . Html.txt_) list
        Markup.CodeBlock list ->
            Html.pre_ (unlines list)
