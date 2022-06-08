--------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Hakyll
import System.FilePath.Posix

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (fromList ["about.rst", "contact.markdown"]) $ do
    route cleanRoute
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defCtx
        >>= cleanIndexUrls

  match "posts/*" $ do
    route cleanRoute
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= cleanIndexUrls

  create ["archive.html"] $ do
    route cleanRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              <> constField "title" "Archives"
              <> defCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= cleanIndexUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              <> defCtx

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= cleanIndexUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    <> defCtx

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
 where
  createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
   where
    p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean)
 where
  idx = "index.html"
  clean url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise = url

urlCtx :: Context a
urlCtx = functionField "getUrl" $
  \args _ -> case args of
    [path] ->
      getRoute (fromFilePath path) >>= \case
        Just route -> return $ toUrl (normalise route)
        Nothing -> fail "unkown path"
    _ -> fail "getUrl takes one argument"

defCtx :: Context String
defCtx = urlCtx <> defaultContext
