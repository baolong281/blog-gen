module Main where

import qualified HsBlog
import qualified HsBlog.Directory as Directory
import qualified HsBlog.Utils as Utils
import Parser

import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = do
    options <- parseOptions
    case options of
        ConvertDir input output _ env ->
            Directory.convertDirectory env input output
        ConvertSingle input output replace ->
            let
                withInputHandle :: (String -> Handle -> IO a) -> IO a
                withInputHandle action =
                    case input of
                        Stdin -> action "" stdin
                        InputFile file ->
                            withFile
                                file
                                ReadMode
                                (action file)

                withOutputHandle :: (Handle -> IO a) -> IO a
                withOutputHandle action =
                    case output of
                        Stdout -> action stdout
                        OutputFile file -> do
                            exists <- doesFileExist file
                            shouldOpenFile <-
                                if exists && not (getBool replace)
                                    then Utils.confirm "File exists, overwrite?"
                                    else pure True
                            if shouldOpenFile
                                then
                                    withFile
                                        file
                                        WriteMode
                                        action
                                else
                                    exitFailure
             in
                withInputHandle (\title -> withOutputHandle . HsBlog.convertSingle title)

getBool :: Replace -> Bool
getBool (Replace b) = b
