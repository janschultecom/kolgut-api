module Main

import Data.List
import Data.String

%language TypeProviders

data Response : Int -> Type where
     Ok : String -> Response 200
     NotFound : Response 404
     NoContent : Response 201

-- makeApiType : Maybe String -> Maybe Type
-- makeApiType (Just x) = Response <$> parsePositive x
-- makeApiType _     = Nothing

-- -- split lines
-- readLines : String -> List String
-- readLines content = let words = Strings.split (\c => List.elem c ['\n', '\r']) content in
--                     filter (/= "") words

-- readAPIType : String -> IO (Provider Type)
-- readAPIType fileName = do maybeFile <- readFile fileName
--                           pure $ case maybeFile of
--                                Left error => Error $ "Failed to load seo words: " ++ show error
--                                Right content => case makeApiType (head' $ readLines content) of
--                     us                 Nothing => Error "Can't read API"
--                                      Just t => Provide t

-- %provide (Api : Type) with readAPIType "specfile"

data Handler : (m : String) -> (p : String) -> (s : Int) -> (a : Type) -> Type where
     Handle : (a -> IO (Response s)) -> Handler m p s a

-- data Api : Type where
--      Nil : Api
--      Handle : String -> String -> (a -> IO (Response s)) -> Api -> Api

runHandler : Handler m p s a -> String -> String -> a -> Maybe (IO (Response s))
runHandler (Handle f) method path x = if m == method && path == p then (Just $ f x) else Nothing

Api : Type
Api = (Handler "GET" "/pets" 200 (),
       Handler "POST" "/pets" 201 ())

toStr : Response a -> String
toStr (Ok r) = r
toStr NoContent =  "Nothing here"

route : Api -> String -> String -> IO String
route (h1, h2) m p = case runMaybe h1 m () of
      Just action => toStr <$> action
      Nothing => pure "run h2!"

-- getPets : IO (Response 200)

-- getPets = pure Ok

getPets : () -> IO (Response 200)
getPets () = pure $ Ok "Dogs"

postPets : () -> IO (Response 201)
postPets () = pure NoContent

api : Api
api = (Handle getPets, Handle postPets)


-- /pets/ -> 200

{-
routes : String -> SwaggerProvider -- HttpStatus 200
routes "pets" = Ok -- compile
routes "pets" = NotFound -- does not compile
-}
