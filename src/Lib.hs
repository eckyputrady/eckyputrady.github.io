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
      , String -- slug (input & output)
      )]
articles = 
  [ ( "Haskell, Redis, Mailgun and Heroku Scheduler"
    , "2017/02/18"
    , "Haskell-Heroku-Mailgun-Redis.md"
    )
  , ( "The 5 Phases Of Startup"
    , "2015/01/01"
    , "The-5-Phases-Of-Startup.md"
    )
  ]

-- | The build pipeline
build = do
  let 
    inputDir = "input"
    outputDir = "output"
    inputPostsDir = inputDir </> "posts"
    stripExtension = fromMaybe "" . headMay . splitElem '.'
    outputPostDir (title, date, slug) = date </> stripExtension slug

  createDirectoryIfMissing True outputDir

  -- create index HTML
  let articleInfo a@(title, date, slug) = (title, date, outputPostDir a)
  writeFile (outputDir </> "index.html") . encodeUtf8 . toStrict
    $ Templates.index (map articleInfo articles)

  -- create post HTML
  let publishPost a@(title, date, slug) = do
        postContent <- readFile $ inputPostsDir </> slug
        let converted = markdown defaultMarkdownSettings $ fromStrict . decodeUtf8 $ postContent
            dir = outputDir </> outputPostDir a
        createDirectoryIfMissing True dir
        writeFile (dir </> "index.html") $ toStrict . encodeUtf8 $ articlePage title date converted
  forM_ articles publishPost

  -- copy various necessary files
  let straightCopy n = copyFile (inputDir </> n) (outputDir </> n)
  mapM_ straightCopy
    [ "index.css"
    , "CNAME"
    , ".gitignore"
    ]
