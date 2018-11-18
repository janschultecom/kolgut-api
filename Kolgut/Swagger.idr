module Kolgut.Swagger

import Kolgut.Json
import Lightyear.StringFile
import Data.String

%access export

rErr : String -> FileError -> String
rErr s f = "Got file error: " ++ s 

pErr : String -> String -> String
pErr s1 s2 = "Got parsing error: \n" ++ s1 ++ s2



data Response : Int -> Type where
     Ok : String -> Response 200
     NotFound : Response 404
     NoContent : Response 201

data HttpMethod = GET | POST 

data Handler : (m : HttpMethod) -> (p : String) -> (s : Int) -> (a : Type) -> Type where
  Handle : (a -> IO (Response s)) -> Handler m p s a


parseResponse : JsonValue -> Either String Int
parseResponse (JsonObject obj) = let tuples = toList obj
                                     codeStr = head' $ map fst tuples 
                                     code = codeStr >>= parsePositive
                                     in
                                        maybeToEither "Invalid status code" code
parseResponse _ = Left "No status codes found"

parseStatusCode : JsonValue -> Either String Int
parseStatusCode (JsonObject obj) = 
  do
    jsonResponse <- maybeToEither "No responses found" $ lookup "responses" obj
    code <- parseResponse jsonResponse
    pure code
parseStatusCode v = Left "Status code empty"


parseMethod : JsonValue -> Either String (List (HttpMethod, Int))
parseMethod (JsonObject obj) = 
  let
   get = map (\v => (GET, v) ) $ lookup "get" obj
   post = map (\v => (POST, v)) $ lookup "post" obj
   methods = (toList get) ++ (toList post)
  in traverse f methods
     where 
       f : (HttpMethod,JsonValue) -> Either String (HttpMethod,Int)
       f (m, v) = map (\code => (m,code)) $ parseStatusCode v

parseMethod v = Left "Method empty"

parseAll : (String, JsonValue) -> Either String (List (String, HttpMethod, Int))
parseAll (path, json) = 
  do 
    methodCodes <- parseMethod json
    pure $ map (\methodCode => (path, (fst methodCode), (snd methodCode))) methodCodes

parsePaths : JsonValue -> Either String (List (String, HttpMethod, Int))
parsePaths (JsonObject obj) = let tuples = toList obj
                                  nested = traverse parseAll tuples 
                                  all = map (\n => n >>= id) nested
                              in all
parsePaths v = Left "Path empty"

toHandlerType : (String,HttpMethod,Int) -> Type
toHandlerType (path,method,code) = Handler method path code ()

-- JsonObject
parseSwagger : JsonValue -> Either String (List Type)
parseSwagger (JsonObject values) = 
  do
    paths <- maybeToEither "No paths found in swagger" $ lookup "paths" values
    triples <- parsePaths paths
    pure $ map toHandlerType triples
     
parseSwagger x = Left "Invalid swagger root"

loadSwagger : IO (Either String (List Type))
loadSwagger = do
          maybeJson <- run $ parseFile rErr pErr jsonToplevelValue "petstore.json"
          pure $ do 
            json <- maybeJson
            types <- parseSwagger json
            pure types
