module Main where

import qualified Convert
import qualified Html
import qualified Markup
import System.Directory (doesFileExist)
import System.Environment (getArgs)

process :: Html.Title -> String -> String
process title = Html.render . Convert.convert title . Markup.parse

main :: IO ()
main = do
    args <- getArgs
    case args of 
