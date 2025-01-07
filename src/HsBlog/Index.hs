module HsBlog.Index where

import HsBlog.Convert (convertStructure)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
    Html.html_
        "Index Page"
        ( Html.h_ 2 (Html.txt_ "Entries")
            <> createPathList files
        )

createPathList :: [(FilePath, Markup.Document)] -> Html.Structure
createPathList files =
    Html.ul_ $ map createPath files
  where
    createPath :: (FilePath, Markup.Document) -> Html.Structure
    createPath (path, doc) = case doc of
        Markup.Heading _ title : rest ->
            Html.h_ 3 (Html.link_ path (Html.txt_ title))
                <> foldMap convertStructure (take 3 rest)
                <> Html.p_ (Html.link_ path (Html.txt_ "..."))
        _ ->
            Html.h_ 3 (Html.link_ path (Html.txt_ path))
