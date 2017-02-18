title: Haskell, Redis, Mailgun and Heroku Scheduler
date: 2017-02-18 21:00:00
tags:
- haskell
- heroku
- redis
- mailgun
---

I recently worked on a Haskell application. One aspect of it is a background job that runs hourly. It connects to Redis and sends email using Mailgun. However, Haskell is not a mainstream language so there are not many tutorials of it. It took me quite a while to figure them all out, so I will write one in case it will save anyone's (e.g. me in the future) time. Oh, and don't worry to try it out since you can do all of this on Heroku free tier.

## Deploying Haskell Application to Heroku

First of all, let's build a fresh Haskell project called `hero` with [stack](https://docs.haskellstack.org/en/stable/README/).

```
stack new hero && cd hero
```

After that, let's push our application as is to Heroku. If you read Heroku documentation, it requires a "Buildpack" to setup and deploy your application. Heroku provides official ones for popular frameworks, like Play or Rails. However, they don't have one for Haskell, so we need either to build one or find one from the community. Fortunately, I found a working [Haskell Buildpack](https://github.com/jackarthurm/heroku-buildpack-stack.git), although it's quite obscure. So we'll use that when creating a Heroku application.

```
# initialize git repo
git init
echo ".stack-work" > .gitignore
git add . && git commit -m "Initial commit"

# create a Heroku application with custom Buildpack
heroku create --buildpack https://github.com/jackarthurm/heroku-buildpack-stack.git
git push heroku master
```

We can verify that we have a working Haskell application on Heroku by running it on a one-off dyno as follow:

```
heroku run /app/.local/bin/hero-exe
```

You should see "someFunc" being printed.

## Setting Up Scheduled Job

Heroku is a fantastic platform. It has quite a number of add-ons. One of it is [Heroku Scheduler](https://elements.heroku.com/addons/scheduler), an add-on to run arbitrary command on a one-off dyno. It's not super flexible like cron, but it's enough for my need. In addition to that, it's totally free, which is awesome. So, we will use this to schedule our application to run hourly.

```
heroku addons:create scheduler:standard
heroku addons:open scheduler
```

The last command should open a page to manage scheduled jobs. Go ahead and add a new job and put `/app/.local/bin/hero-exe +RTS -N` as the command to run. The `+RTS -N` is a parameter that you give to your Haskell application to enable parallelism by using as many cores as the machine has. Based on my experience, your application will run on an 8 cores machine by Heroku.

Wait until the time your job should be run by the scheduler and verify it by reading the logs:

```
heroku logs -d scheduler
```

You should see that "someFunc" is printed, indicating that your application has been scheduled successfully.

## Connecting to Redis

Heroku provides Redis in a form of an [add-on](https://elements.heroku.com/addons/heroku-redis). It has a generous free plan which gives you 25MB of storage. It, however, doesn't persist your data on disk. Well, if you use Redis, mostly your use cases are caching. So that should not be a big deal anyway. Let's add that to our application.

```
heroku addons:create heroku-redis:hobby-dev
```

After installing Redis add-on, your application will have an environment variable called `REDIS_URL` that you can use to connect to Redis. The format of the value is `redis://<user>:<password>@<domain>:<port>`. You could just ignore the `user` part since Redis doesn't have a multi-user access feature.

One Haskell library that I have used successfully to connect to Redis is [hedis](https://hackage.haskell.org/package/hedis). It's simple to use and has a nice tutorial to get you started. Let's start writing some code for connecting to Redis.

```
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import qualified Database.Redis as Redis
import System.Environment
import Network.URI
import Data.Maybe
import Data.List.Split
import qualified Data.ByteString.Char8 as BS

someFunc :: IO ()
someFunc = do
  putStrLn "testing redis"
  testRedis
  putStrLn "testing redis done"

testRedis :: IO ()
testRedis = do
  conn <- redisConn
  hello <- Redis.runRedis conn $ do
    Redis.set "hello" "world"
    Redis.get "hello"
  putStrLn $ show hello

redisConn :: IO Redis.Connection
redisConn = do
  env <- getEnv "REDIS_URL" -- redis://user:pass@host:port
  let uri = fromJust $ parseURI env
  let auth = fromJust $ uriAuthority uri
  let host = uriRegName auth
  let port = tail $ uriPort auth
  let pass = uriUserInfo auth |> splitOn ":" |> \xs -> xs !! 1 |> init |> BS.pack
  Redis.checkedConnect $ Redis.defaultConnectInfo
    { Redis.connectHost = host
    , Redis.connectPort = Redis.PortNumber (read port)
    , Redis.connectAuth = Just pass
    }

(|>) :: a -> (a -> b) -> b
a |> f = f a
```

Before compiling the application, you need to add few dependencies in your `hero.cabal` file. `hedis` is obviously needed, but you also need `split`, `bytestring` and `network-uri` as well. They are used to parse the `REDIS_URL` value.

Now let's push that into Heroku to test whether it connects correctly.

```
git commit -am "Test Redis"
git push heroku master
heroku run /app/.local/bin/hero-exe
```

You should see the following output indicating that the application has connected correctly to Redis.

```
testing redis
Right (Just "world")
testing redis done
```

## Sending Email via Mailgun

Before discovering Mailgun, I used to interface with SMTP directly. Boy, it was a [PITA](http://www.urbandictionary.com/define.php?term=P.I.T.A). Few problems arose like your mail provider doesn't give access because your application is not considered secure and the Haskell library in this area is not good. Mailgun eases the email sending by allowing you to do it via REST API. They also give you a starter domain for testing. So it almost works out of the box, basically.

Haskell, fortunately, has a Mailgun wrapper library called [hailgun](https://hackage.haskell.org/package/hailgun). The documentation is not as good as hedis since it doesn't give you any tutorial to get started. It's quite common in Haskell-land for a library to be documented poorly. However, this library is quite simple that if you just follow the types, you can figure it out easily what to do. But still, a bit of tutorial could save me time here.

So, after setting up a Mailgun account, let's write some code to test it:

```
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Mail.Hailgun
import Data.Either.Unwrap

someFunc :: IO ()
someFunc = do
  putStrLn "testing mail"
  testEmail
  putStrLn "testing mail done"

testEmail :: IO ()
testEmail = sendEmail hailgunCtx hailgunMsg >>= (putStrLn . show)

hailgunCtx =
  HailgunContext "get.domain.from.mailgun"
                 "get.key.from.mailgun"
                 Nothing

hailgunMsg = fromRight $
  hailgunMessage "Test Subject"
                 (TextOnly "Test content")
                 "address.for.respond@gmail.com"
                 emptyMessageRecipients { recipientsTo = ["recipient@gmail.com"] }
                 []
```

You need to add `hailgun` and `either-unwrap` in your `hero.cabal` file. We don't need to run this in Heroku for starter. Let's just run it in GHCi and call the function directly:

```
stack ghci
*Main> someFunc
```

You should receive the email in the email you listed as the recipient. You will also get the following output indicating that we have connected succesfully to Mailgun.

```
testing mail
Right (HailgunSendResponse {hsrMessage = "Queued. Thank you.", hsrId = "<xxxxx>"})
testing mail done
```

Obviously, you don't want to hardcode your Mailgun credentials in your source code. The best practice is to put it in the environment variables. In Heroku, you can add an environment variable via `heroku config:set <KEY>=<VALUE>` command. We can adjust our code accordingly to read the credentials from the environment variables instead.

## Closing

In this blog post, I have shown you how Haskell can be used for building practical application. All the code above is not intended as production-ready code as it doesn't handle errors gracefully. I keep it that way for simplicity.

That's it, folks! Hope it helps.