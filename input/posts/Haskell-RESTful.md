## Introduction

Despite being infamously known as impractical programming language with steep learning curve, Haskell is a good choice for building web applications. The ecosystem is mature enough so that you can find packages to support web development.

In this article, we will be building a simple CRUD RESTful API with Haskell. Our API will manage a todo list. We can add, remove, update and read our todos. Our API follows the API blueprint specification below:

```bash hljs
FORMAT: 1A

# Todos API

Todos API is a todo storage backend for [TodoMVC](//todomvc.com).

# Group Todos

# Todos Collection [/todos]

## Create a Todo [POST]

+ Request (application/json)

            {
                "title": "dredd"
            }

+ Response 201 (application/json; charset=utf-8)

            {
                "id": 42,
                "title": "dredd",
                "completed": false
            }

## List all Todos [GET]

+ Response 200 (application/json; charset=utf-8)

            [{
                "id": 42,
                "title": "dredd",
                "completed": false
            }]

## Archive completed Todos [DELETE]

+ Response 204

# Todo [/todos/{id}]

+ Parameters
  + id (required, number, `42`)

## Get a Todo [GET]

+ Response 200 (application/json; charset=utf-8)

            {
                "id": 42,
                "title": "dredd",
                "completed": false
            }

## Update a Todo [PUT]

+ Request (application/json)

            {
                "title": "dredd",
                "completed": true
            }

+ Response 200 (application/json; charset=utf-8)

            {
                "id": 42,
                "title": "dredd",
                "completed": true
            }

## Delete a Todo [DELETE]

+ Response 204
```

## Project Setup

Let's start by creating a new project with `stack`.

```bash hljs
$ stack new todomvc-api
$ cd todomvc-api
```

Next, we modify the `package.yaml` file to include the following packages:

```yaml hljs
dependencies:
- base
- classy-prelude
- lens
- generic-lens

library:
  source-dirs: src
  dependencies:
  - aeson
  - scotty
  - wai
  - http-types
  - data-has
```

We use `classy-prelude` as a replacement for default prelude. `classy-prelude` has many benefits over the default one. The one that we reap the most in this article is being able to use various common modules with one single import.

We also import `lens` and `generic-lens`. Those libraries helps us in accessing and modifying data structure.

`aeson` will be used for JSON serialization and deserialization.

For handling HTTP requests, we will use `scotty`, `wai` and `http-types`. `scotty` mostly handles the routing side of the application, while `wai` and `http-types` are only providing us few supporting functions and types.

`data-has` is a small but useful package that allows as to express "we can get type A from r". This might not seem makes sense now, but we will see how it will be used later.

In addition to package dependencies, we also lists the following language extensions:

```yaml hljs
default-extensions:
- OverloadedStrings
- GeneralizedNewtypeDeriving
- ConstraintKinds
- FlexibleContexts
- NoImplicitPrelude
- DataKinds
- DeriveGeneric
- DuplicateRecordFields
- TypeApplications
- ScopedTypeVariables
```

## Defining Types

Let's move on to defining the types. We will define our types in `Todo.Types` module. Let's type in the module definition as well as the required imports.

```haskell hljs
module Todo.Types where

import ClassyPrelude
import Data.Aeson
import Data.Generics.Product
import GHC.Generics (Generic)
```

From the API blueprint, we may infer that there are 3 types. The first one represents the request to create a todo:

```haskell hljs
newtype CreateTodo = CreateTodo
  { title :: Text
  } deriving (Generic, Show)

instance ToJSON CreateTodo
instance FromJSON CreateTodo
```

The second one represents the request to update a todo:

```haskell hljs
data UpdateTodo = UpdateTodo
  { title :: Text
  , completed :: Bool
  } deriving (Generic, Show)

instance ToJSON UpdateTodo
instance FromJSON UpdateTodo
```

And finally, the todo object itself:

```haskell hljs
data Todo = Todo
  { id :: Int
  , title :: Text
  , completed :: Bool
  } deriving (Generic, Show)

instance ToJSON Todo
instance FromJSON Todo
```

We also define `ToJSON` and `FromJSON` instances for each of the types. This way, they can be serialized and deserialized to JSON.

## Implementing Services

We will now create a module that implements all operation to support the API, such as creating a todo and reading all todos. We name the module as `Todo.Service`:

```haskell hljs
module Todo.Service where

import ClassyPrelude hiding (snoc)
import Todo.Types
import Control.Lens
import Data.Has
import Data.Generics.Product
```

Since we want to store the created todos, let's do with something simple for now: just store it in-memory. The data structure where the todos are stored is defined like so:

```haskell hljs
data State = State
  { lastId :: Int
  , todos :: [Todo]
  } deriving (Generic, Show)

initialState :: State
initialState = State { lastId = 0, todos = [] }
```

Every `Todo` has an ID that we need to generate. To ensure that there's no duplicated ID, we generate it in increasing order. `lastId` field keeps track of the last generated ID. With this value at hand, generating a new ID is just incrementing this value.

`todos` is a list of `Todo`. This is the place where we store all of the `Todo`s that our application currently has.

Now let's list all of the required operations:

```haskell hljs
type Deps r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

addTodo :: Deps r m => CreateTodo -> m Todo
removeCompletedTodos :: Deps r m => m ()
getAllTodos :: Deps r m => m [Todo]
getTodo :: Deps r m => Int -> m (Maybe Todo)
updateTodo :: Deps r m => Todo -> m (Maybe Todo)
removeTodo :: Deps r m => Int -> m ()
```

Each operation maps exactly to each endpoint as specified in the blueprint above. For each operation, we constraint it to `Deps r m`. This constraint says that the operation may perform IO (`MonadIO`) and able to get `TVar State` from the environment (`Has (TVar State) r, MonadReader r m`).

We will implement each function. Let's start from `addTodo`:

```haskell hljs
withTVar :: Deps r m => (TVar State -> STM a) -> m a
withTVar f = do
  tvar <- asks getter
  atomically $ f tvar

addTodo :: Deps r m => CreateTodo -> m Todo
addTodo createTodo = withTVar $ \tvar -> do
  state <- readTVar tvar
  let newId = 1 + state ^. field @"lastId"
      newTodo = 
        Todo  { id = newId
              , title = createTodo ^. field @"title"
              , completed = False
              }
      newState = 
        State { lastId = newId
              , todos = snoc (state ^. field @"todos") newTodo
              }
  writeTVar tvar newState
  return newTodo
```

We first define a utility function `withTVar`. This function gets the `TVar` from the environment and execute the supplied function within the `STM` monad. This routine is necessary for all functions, hence it makes sense to create a utility function to do this to reduce code duplication.

In the `addTodo` function, we first read the `State` from `TVar`. Then, we generate a new ID for the new `Todo`. The new ID is generated by incrementing the last generated ID. We keep track the last generated ID in the `lastId` field in the `State` data structure. After that, we create a `Todo` by supplying the newly generated ID and the title from `CreateTodo` from the parameter. The newly created `Todo` is then appended to the existing `todos` field, using `snoc` function. `snoc` is a function that adds an element to the end of the list. Finally, we write the `State` back to the `TVar` and return the newly created `Todo`.

```haskell hljs
removeCompletedTodos :: Deps r m => m ()
removeCompletedTodos = withTVar $ \tvar -> 
  modifyTVar' tvar $ \state ->
    state & field @"todos" %~ (filter (not . getField @"completed"))
```

In `removeCompletedTodos` function, we use `modifyTVar'` function to replace the state with a new one. `state & field @"todos" %~ f` is used to apply function `f` to `todos` field in `state`, and create a new `State` structure with `todos` field replaced by the result of that function application. The `(%~)` and `(&)` functions become available once we import `Control.Lens` module. The function that we apply is `(filter (not . getField @"completed"))`, which basically says that we filter the `todos` list so that only `Todo`s that are not yet completed remains.

```haskell hljs
getAllTodos :: Deps r m => m [Todo]
getAllTodos = withTVar $ \tvar -> do
  state <- readTVar tvar
  return $ state ^. field @"todos"
```

`getAllTodos` is pretty straightforward, we just read the `State` from the `TVar`, and return the `todos` field.

```haskell hljs
getTodo :: Deps r m => Int -> m (Maybe Todo)
getTodo todoId = do
  todos <- getAllTodos
  return $ find (\todo -> todo ^. field @"id" == todoId) todos
```

`getTodo` reuses `getAllTodos` and find a `Todo` that has the given ID. Since it's possible that the caller gives bad ID, the return value is wrapped in a `Maybe`. `Nothing` will indicates that the `Todo` with the given ID is not found in our repository.

```haskell hljs
updateTodo :: Deps r m => Todo -> m (Maybe Todo)
updateTodo newTodo = withTVar $ \tvar -> do
  state <- readTVar tvar
  let existingTodos = state ^. field @"todos"
      todoId = newTodo ^. field @"id"
      mayTodo = find (\todo -> todo ^. field @"id" == todoId) existingTodos
  case mayTodo of
    Nothing ->
      return Nothing
    Just _ -> do
      let replace todo =
            if todo ^. field @"id" == todoId
            then newTodo
            else todo
          newState = state & field @"todos" . traverse %~ replace
      writeTVar tvar newState
      return $ Just newTodo
```

The `updateTodo` function is a bit more complicated then what we have encountered previously. We first get the `State` from the `TVar`. Then we check whether the ID presents in our `State` or not. If it is not present, then we return `Nothing` since there is nothing to update. Otherwise, we apply a utility function, `replace`, to each item in the `todos` field. The `replace` function receives a `Todo` and will return the `Todo` from the function if the ID doesn't match, otherwise return `newTodo` if the ID matches.

```haskell hljs
removeTodo :: Deps r m => Int -> m ()
removeTodo todoId = withTVar $ \tvar ->
  modifyTVar' tvar $ \state ->
    state & field @"todos" %~ (filter (\todo -> todo ^. field @"id" /= todoId))
```

In `removeTodo` function, we filter the items `todos` so that no item has `id` equals to the given `todoId`. This effectively removes the `Todo` with the same ID from our state, if such `Todo` exists.

## Routes

We need to expose the functions defined in the `Todo.Service` module over HTTP. In order to do that, we define `Todo.Routes` module. This module is in charge of handling HTTP requests, translating that to function calls in `Todo.Service` module, and translating the result back to HTTP responses.

```haskell hljs
module Todo.Routes where

import ClassyPrelude hiding (delete)
import Control.Lens
import Data.Generics.Product hiding (param)
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai
import Todo.Types
import qualified Todo.Service as S
```

Let's define a skeleton for routes according to the API specification.

```haskell hljs
routes :: S.Deps r m => ScottyT LText m ()
routes = do
  post "/todos" undefined
  get "/todos" undefined
  delete "/todos" undefined
  get "/todos/:id" undefined
  put "/todos/:id" undefined
  delete "/todos/:id" undefined
```

Let's implement each route starting from the top.

```haskell hljs
  post "/todos" $ do
    arg <- jsonData
    result <- lift $ S.addTodo arg
    status status201
    json result
```

The code snippet above handles `POST /todos` HTTP request. As the API spec says, this API endpoint is used to add a new `Todo`. We read the JSON payload from the request and pass that to `addTodo` function. Our application knows how to parse JSON into a `CreateTodo` struct because we have defined a `FromJSON` instance for `CreateTodo` in `Todo.Types` module. The result of `addTodo` function is then returned as JSON. In this case, our application knows how to convert `Todo` to JSON because we have defined a `ToJSON` instance for `Todo` in `Todo.Types` module. In addition to that, we also set the HTTP status response to 201.

```haskell hljs
  get "/todos" $ do
    result <- lift $ S.getAllTodos
    json result

  delete "/todos" $ do
    lift $ S.removeCompletedTodos
    status status204
```

The two routes above define handler for `GET /todos` and `DELETE /todos`. As you can see, the implementation is very straightforward. They don't need to read any parameter from HTTP request. They just need to invoke the correct functions in `Todo.Service` module and translate the result to HTTP response accordingly.

```haskell hljs
  get "/todos/:id" $ do
    todoId <- param "id"
    mayResult <- lift $ S.getTodo todoId
    case mayResult of
      Nothing ->
        status status404
      Just result ->
        json result
```

The `GET /todos/:id` endpoint is used to get specific `Todo` item according to the ID path parameter. We parse the ID path parameter using `param "id"`. The parsed ID is then passed to `S.getTodo` function. The ID may not always correct. We might receive bogus ID so that there's no matching `Todo` found in our repository. On this scenario, `S.getTodo` function will return `Nothing`, in which we translate that to HTTP status 404. On the other hand, if we do find a `Todo`, then we return that as JSON.

```haskell hljs
  put "/todos/:id" $ do
    todoId <- param "id"
    arg :: UpdateTodo <- jsonData
    let todo = Todo { id = todoId
                    , title = arg ^. field @"title"
                    , completed = arg ^. field @"completed"
                    }
    mayResult <- lift $ S.updateTodo todo
    case mayResult of
      Nothing ->
        status status404
      Just result ->
        json result
```

`PUT /todos` endpoint is a bit trickier. We need to combine path parameter and JSON from HTTP request body into a `Todo` data structure. Once we have constructed `Todo`, we pass that to `S.updateTodo` function. We then translate the result to either a 404 or 200 with JSON payload.

```haskell hljs
  delete "/todos/:id" $ do
    todoId <- param "id"
    lift $ S.removeTodo todoId
    status status204
```

The `DELETE /todos/:id` endpoint is used delete a `Todo` item. For this endpoint, we just need to parse ID path parameter and pass that to `S.removeTodo` function.

## Main

We define `Lib` module. This module basically kickstart the application into action.

```haskell hljs
module Lib (main) where

import ClassyPrelude
import Todo.Routes as TodoR
import Todo.Service as TodoS
import Web.Scotty.Trans
```

Next, we define our application's monad transformer stack. We know that our application needs to have access to `TVar TodoS.State` from the environment and able to perform `IO` actions. So, we just need a `ReaderT` over `IO`.

```haskell hljs
type Env = TVar TodoS.State
type App a = ReaderT Env IO a

runApp :: Env -> App a -> IO a
runApp = flip runReaderT
```

Our `main` function is the function that actually starts the application:

```haskell hljs
main :: IO ()
main = do
  env <- newTVarIO TodoS.initialState
  let runner = runApp env
  scottyT 3000 runner TodoR.routes
```

We create a new `TVar` with `newTVarIO`. We initialize it with the initial state of the application that we define with `TodoS.initialState` function. Then, we define `runner`. This is a function that transforms our monad (`App`) into `IO`. This transformer function is required as the second parameter of `scottyT` function. The last line of the above snippet starts the server on port 3000, with the `runner` transformer function and `TodoR.routes` as the HTTP request handler.

Finally, we need to edit `app/Main.hs` to call the `main` function we have just defined:

```haskell hljs
module Main where

import ClassyPrelude
import qualified Lib

main :: IO ()
main = Lib.main
```

Congratulations, we have finished our API implementation in Haskell!

## Test Drive

Let's see our application in action. Build and run the application with the following command:

```bash hljs
$ stack build
$ stack exec todomvc-api-exe
```

Let's open other terminal and issue various `curl` commands. We start by adding 3 new todo items.

```bash hljs
$ curl --request POST \
>   --url http://localhost:3000/todos \
>   --header 'content-type: application/json' \
>   --data '{ "title": "TODO 1" }'
{"completed":false,"id":1,"title":"TODO 1"}

$ curl --request POST \
>   --url http://localhost:3000/todos \
>   --header 'content-type: application/json' \
>   --data '{ "title": "TODO 2" }'
{"completed":false,"id":2,"title":"TODO 2"}

$ curl --request POST \
>   --url http://localhost:3000/todos \
>   --header 'content-type: application/json' \
>   --data '{ "title": "TODO 3" }'
{"completed":false,"id":3,"title":"TODO 3"}
```

Let's see all of the todo items we have so far.

```bash hljs
curl --request GET \
>   --url http://localhost:3000/todos \
>   --header 'content-type: application/json'
[
  {"completed":false,"id":1,"title":"TODO 1"},
  {"completed":false,"id":2,"title":"TODO 2"},
  {"completed":false,"id":3,"title":"TODO 3"}
]
```

As we can see, the application correctly store our 3 todo items.

```bash hljs
$ curl --request GET \
>   --url http://localhost:3000/todos/1
{"completed":false,"id":1,"title":"TODO 1"}
```

The get single todo item endpoint also work correctly.

Let's try updating one of our todo item.

```bash hljs
$ curl --request PUT \
>   --url http://localhost:3000/todos/1 \
>   --header 'content-type: application/json' \
>   --data '{ "title": "TODO 1 completed", "completed": true }'
{"completed":true,"id":1,"title":"TODO 1 completed"}

$ curl --request GET
>   --url http://localhost:3000/todos
>   --header 'content-type: application/json'
[
  {"completed":true,"id":1,"title":"TODO 1 completed"},
  {"completed":false,"id":2,"title":"TODO 2"},
  {"completed":false,"id":3,"title":"TODO 3"}
]
```

We set our todo item with ID 1 to be completed. We also change the title.

Let's now remove all completed items.

```bash hljs
$ curl --request DELETE \
>   --url http://localhost:3000/todos \
>   --header 'content-type: application/json'
>   -v
*   Trying ::1...
* TCP_NODELAY set
* Connection failed
* connect to ::1 port 3000 failed: Connection refused
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 3000 (#0)
> DELETE /todos HTTP/1.1
> Host: localhost:3000
> User-Agent: curl/7.54.0
> Accept: */*
> content-type: application/json
>
< HTTP/1.1 204 No Content
< Date: Sun, 16 Sep 2018 08:43:59 GMT
< Server: Warp/3.2.23
<
* Connection #0 to host localhost left intact
```

The endpoint above does not return any response body. So, we set `curl` in verbose mode to inspect the resulting HTTP status. `204 No Content` is the HTTP status that we expect.

```bash hljs
$ curl --request GET
>   --url http://localhost:3000/todos
>   --header 'content-type: application/json'
[
  {"completed":false,"id":2,"title":"TODO 2"},
  {"completed":false,"id":3,"title":"TODO 3"}
]
```

The completed todo item is indeed removed when we get all todos.

Finally, let's try to remove one.

```bash hljs
$ curl --request DELETE \
>   --url http://localhost:3000/todos/2 \
>   --header 'content-type: application/json'
>   -v
*   Trying ::1...
* TCP_NODELAY set
* Connection failed
* connect to ::1 port 3000 failed: Connection refused
*   Trying 127.0.0.1...
* TCP_NODELAY set
* Connected to localhost (127.0.0.1) port 3000 (#0)
> DELETE /todos/2 HTTP/1.1
> Host: localhost:3000
> User-Agent: curl/7.54.0
> Accept: */*
> content-type: application/json
>
< HTTP/1.1 204 No Content
< Date: Sun, 16 Sep 2018 08:48:13 GMT
< Server: Warp/3.2.23
<
* Connection #0 to host localhost left intact
```

As expected, the endpoint responds with 204.

```bash hljs
$ curl --request GET
>   --url http://localhost:3000/todos
>   --header 'content-type: application/json'
[
  {"completed":false,"id":3,"title":"TODO 3"}
]
```

We also verify that the todo item with ID 2 is also removed from our todo items list.

## Closing

In this article, we have seen how a simple CRUD RESTful API is built with Haskell. Thanks to Haskell's terseness and type system, we can do a lot with few lines of code.

While our application works, there are, however, other areas that this article does not yet touch:

1. Input validation -- How do we respond with friendlier error message if client sends a malformed request?
2. Logging -- How to setup a proper logging format so that we can troubleshoot production issues?
3. Databases -- How should we store data so that it survives application reset?
4. Tests -- How to write tests? How should we structure our code so that they are loosely coupled and easier to test?
5. Deployment -- How should we package our Haskell application and ship it to production servers?
6. Static code analysis -- What are the available code quality tools that we can apply to our codebase so that we are more confident that our application is defect-free and maintainable?

Those topics are worth exploring and important to learn.

In case you want to see the complete source code for application that we have just built, the code is hosted on [github](https://github.com/eckyputrady/todomvc-api).