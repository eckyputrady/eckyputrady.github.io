module Lib
    ( someFunc
    ) where

import ClassyPrelude
import Templates
import System.Directory
import Control.Concurrent (forkIO)

import Text.Markdown

someFunc :: IO ()
someFunc = build

-- | All articles and will be displayed in the homepage according to the way it is sorted as is
articles 
  :: [( String -- title
      , String -- date
      , String -- output filename
      , String -- input filename
      )]
articles = 
  [ ( "Haskell, Redis, Mailgun and Heroku Scheduler"
    , "2017-02-18"
    , "haskell-redis-mailgun-heroku-scheduler.html"
    , "heroku.md"
    )
  , ( "The 5 Phases Of Startup"
    , "2015-01-01"
    , "the-5-phases-of-startup.html"
    , "startup.md"
    )
  ]

-- | The build pipeline
build = do
  let 
    inputDir = "input"
    outputDir = "output"
    inputPostsDir = inputDir </> "posts"
    outputPostsDir = outputDir </> "posts"

  createDirectoryIfMissing True outputDir

  -- create index HTML
  let articleInfo (t, d, o, _) = (t, d, "posts" </> o)
  writeFile (outputDir </> "index.html") . encodeUtf8 . toStrict
    $ Templates.index (map articleInfo articles)

  -- create post HTML
  let 
    publishPost (title, date, outputFile, inputFile) = do
      postContent <- readFile $ inputPostsDir </> inputFile
      let converted = markdown defaultMarkdownSettings $ fromStrict . decodeUtf8 $ postContent
      writeFile (outputPostsDir </> outputFile) $ toStrict . encodeUtf8 $ articlePage title date converted
  createDirectoryIfMissing True outputPostsDir
  forM_ articles publishPost

  -- copy various necessary files
  let straightCopy n = copyFile (inputDir </> n) (outputDir </> n)
  straightCopy "index.css"
  straightCopy "CNAME"
  straightCopy ".gitignore"
