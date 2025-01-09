module HsBlog (
    process,
    convertSingle,
)
where

import qualified HsBlog.Convert as Convert
import HsBlog.Env
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import System.IO (Handle, hGetContents, hPutStrLn)

process :: Html.Title -> String -> String
process title = Html.render . Convert.convert defaultEnv title . Markup.parse

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
    content <- hGetContents input
    hPutStrLn output (process title content)
