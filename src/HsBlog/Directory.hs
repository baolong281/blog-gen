module HsBlog.Directory (buildIndex, convertDirectory)
where

import HsBlog.Convert (convert, convertStructure)
import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import qualified HsBlog.Utils as Utils

import Control.Monad (void, when)
import Data.List (partition)
import Data.Traversable (for)

import Control.Exception (SomeException (..), catch, displayException)
import System.Directory (
    copyFile,
    createDirectory,
    doesDirectoryExist,
    listDirectory,
    removeDirectoryRecursive,
 )
import System.Exit (exitFailure)
import System.FilePath (
    takeBaseName,
    takeExtension,
    takeFileName,
    (<.>),
    (</>),
 )
import System.IO (hPutStrLn, stderr)

data DirContents
    = DirContents
    { dcFilesToProcess :: [(FilePath, String)]
    , dcFilesToCopy :: [FilePath]
    }

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory inputDir outputDir = do
    DirContents filesToProcess filesToCopy <- getDirFilesAndContent inputDir
    createOutputDirectoryOrExit outputDir
    let
        outputHtmls = txtsToRenderedHtml filesToProcess
    copyFiles outputDir filesToCopy
    writeFiles outputDir outputHtmls
    putStrLn "Done."

copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outputDir files = do
    let
        copyFromTo file = copyFile file (outputDir </> takeFileName file)
    void $ applyIoOnList copyFromTo files >>= filterAndReportFailures

writeFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeFiles outputDir files = do
    let
        writeFileContent (file, content) =
            writeFile (outputDir </> file) content
    void $ applyIoOnList writeFileContent files >>= filterAndReportFailures

getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent inputDir = do
    files <- map (inputDir </>) <$> listDirectory inputDir
    let
        (txtFiles, otherFiles) =
            partition ((== ".txt") . takeExtension) files
    txtFilesAndContent <-
        applyIoOnList readFile txtFiles >>= filterAndReportFailures
    pure $
        DirContents
            { dcFilesToProcess = txtFilesAndContent
            , dcFilesToCopy = otherFiles
            }

applyIoOnList :: (a -> IO b) -> [a] -> IO [(a, Either String b)]
applyIoOnList action inputs =
    for inputs $ \input -> do
        result <-
            catch
                (Right <$> action input)
                ( \(SomeException e) -> do
                    pure $ Left (displayException e)
                )
        pure (input, result)

filterAndReportFailures :: [(a, Either String b)] -> IO [(a, b)]
filterAndReportFailures =
    foldMap $ \(file, contentOrErr) ->
        case contentOrErr of
            Left err -> do
                hPutStrLn stderr err
                pure []
            Right content ->
                pure [(file, content)]

createOutputDirectoryOrExit :: FilePath -> IO ()
createOutputDirectoryOrExit outputDir =
    whenIO
        (not <$> createOutputDirectory outputDir)
        (hPutStrLn stderr "Cancelled." *> exitFailure)

createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory dir = do
    dirExists <- doesDirectoryExist dir
    create <-
        if dirExists
            then do
                override <- Utils.confirm "Output directory exists. Override?"
                when override (removeDirectoryRecursive dir)
                pure override
            else
                pure True
    when create (createDirectory dir)
    pure create

whenIO :: IO Bool -> IO () -> IO ()
whenIO bool action = do
    condition <- bool
    when condition action

buildIndex :: [(FilePath, Markup.Document)] -> Html.Html
buildIndex files =
    Html.html_
        "Index Page"
        ( Html.h_ 2 (Html.txt_ "Entries")
            <> createPathList files
        )
txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml files =
    let
        outputFiles = map toOutputMarkupFile files
        index = ("index.html", buildIndex outputFiles)
     in
        map (fmap Html.render) (index : map convertFile outputFiles)

toOutputMarkupFile :: (FilePath, String) -> (FilePath, Markup.Document)
toOutputMarkupFile (file, content) =
    (takeBaseName file <.> ".html", Markup.parse content)

convertFile :: (FilePath, Markup.Document) -> (FilePath, Html.Html)
convertFile (path, document) =
    (path, convert path document)

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
