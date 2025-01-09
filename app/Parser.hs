module Parser (
    Options (..),
    SingleInput (..),
    SingleOutput (..),
    Replace (..),
    parseOptions,
)
where

import Data.Maybe (fromMaybe)
import HsBlog.Env (Env (Env, eBlogName, eStylesheet), defaultEnv)
import Options.Applicative

parseOptions :: IO Options
parseOptions = execParser opts

-------------------------------------------------------------------------------
-- options models

data Options
    = ConvertSingle SingleInput SingleOutput Replace
    | ConvertDir FilePath FilePath Replace Env

data SingleInput
    = Stdin
    | InputFile FilePath
    deriving (Show)

data SingleOutput
    = Stdout
    | OutputFile FilePath
    deriving (Show)

newtype Replace = Replace Bool deriving (Show)

-------------------------------------------------------------------------------
-- main parser and options

pOptions :: Parser Options
pOptions =
    subparser
        ( command
            "convert"
            ( info
                (helper <*> pConvertSingle)
                (progDesc "Convert a single markup source to html")
            )
            <> command
                "convert-dir"
                ( info
                    (helper <*> pConvertDir)
                    (progDesc "Convert a directory of markup files to html")
                )
        )

opts :: ParserInfo Options
opts =
    info
        (helper <*> pOptions)
        ( fullDesc
            <> header "hs-blog-gen - a static blog generator"
            <> progDesc "Convert markup files or directories to html"
        )

-------------------------------------------------------------------------------
-- parsing single files and stdin

pConvertSingle :: Parser Options
pConvertSingle =
    ConvertSingle <$> pSingleInput <*> pSingleOutput <*> pReplace

pSingleInput :: Parser SingleInput
pSingleInput =
    fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput =
    fromMaybe Stdout <$> optional pOutputFile

pInputFile :: Parser SingleInput
pInputFile = fmap InputFile inp

pOutputFile :: Parser SingleOutput
pOutputFile = fmap OutputFile outp

inp :: Parser FilePath
inp =
    strOption
        ( long "input"
            <> short 'i'
            <> metavar "FILE"
            <> help "Input file"
        )

outp :: Parser FilePath
outp =
    strOption
        ( long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output file"
        )

-------------------------------------------------------------------------------
-- parsing directories

pConvertDir :: Parser Options
pConvertDir =
    ConvertDir <$> pInputDir <*> pOutputDir <*> pReplace <*> pEnv

pInputDir :: Parser FilePath
pInputDir =
    strOption
        ( long "input"
            <> short 'i'
            <> metavar "DIRECTORY"
            <> help "Input directory"
        )

pOutputDir :: Parser FilePath
pOutputDir =
    strOption
        ( long "output"
            <> short 'o'
            <> metavar "DIRECTORY"
            <> help "Output directory"
        )

pEnv :: Parser Env
pEnv =
    Env <$> pBlogName <*> pStylesheet

pBlogName :: Parser String
pBlogName =
    strOption
        ( long "name"
            <> short 'n'
            <> metavar "STRING"
            <> help "Blog name"
            <> value (eBlogName defaultEnv)
            <> showDefault
        )

pStylesheet :: Parser FilePath
pStylesheet =
    strOption
        ( long "style"
            <> short 'S'
            <> metavar "FILE"
            <> help "Stylesheet filename"
            <> value (eStylesheet defaultEnv)
            <> showDefault
        )

-------------------------------------------------------------------------------
-- parsing replace option

pReplace :: Parser Replace
pReplace =
    Replace <$> pReplaceBool

pReplaceBool :: Parser Bool
pReplaceBool =
    switch
        ( long "replace"
            <> short 'r'
            <> help "Automatically replace files/directories if they already exist"
        )
