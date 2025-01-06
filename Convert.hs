module Convert where

import qualified Html
import qualified Markup

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
    case structure of
        Markup.Heading n txt ->
            Html.h_ n txt
        Markup.Paragraph txt ->
            Html.p_ txt
        Markup.UnorderedList list ->
            Html.ul_ $ map Html.p_ list
        Markup.OrderedList list ->
            Html.ol_ $ map Html.p_ list
        Markup.CodeBlock list ->
            Html.pre_ (unlines list)