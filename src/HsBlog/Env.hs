module HsBlog.Env where

import qualified HsBlog.Html as Html

data Env
    = Env
    { eBlogName :: String
    , eStylesheet :: FilePath
    }

defaultEnv :: Env
defaultEnv = Env "Blog" "styles.css"

headFromEnv :: Env -> String -> Html.Head
headFromEnv env title =
    Html.title_ (eBlogName env <> " - " <> title)
        <> Html.stylesheet_ (eStylesheet env)
