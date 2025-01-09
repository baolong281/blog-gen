module HsBlog.Convert where

import HsBlog.Env
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

convert :: Env -> String -> Markup.Document -> Html.Html
convert env title doc =
    let
        doc_head = headFromEnv env title
        processed = foldMap convertStructure doc
     in
        Html.html_ doc_head processed

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
