module Kolgut.Swagger

import Kolgut.Json
import Lightyear.StringFile
import Lightyear.Strings 
import Data.String
import Debug.Error

%access public export
%language ElabReflection

rErr : String -> FileError -> String
rErr s f = "Got file error: " ++ s

pErr : String -> String -> String
pErr s1 s2 = "Got parsing error: \n" ++ s1 ++ s2


data Response : Int -> Type where
     Ok : String -> Response 200
     NotFound : Response 404
     NoContent : Response 201
     InternalServerError : Response 503

data Handler : (m : String) -> (p : String) -> (s : Int) -> (a : Type) -> Type where
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


parseMethod : JsonValue -> Either String (List (String, Int))
parseMethod (JsonObject obj) =
  let
   get = map (\v => ("GET", v) ) $ lookup "get" obj
   post = map (\v => ("POST", v)) $ lookup "post" obj
   methods = (toList get) ++ (toList post)
  in traverse f methods
     where
       f : (String,JsonValue) -> Either String (String,Int)
       f (m, v) = map (\code => (m,code)) $ parseStatusCode v

parseMethod v = Left "Method empty"

parseAll : (String, JsonValue) -> Either String (List (String, String, Int))
parseAll (path, json) =
  do
    methodCodes <- parseMethod json
    pure $ map (\methodCode => (path, (fst methodCode), (snd methodCode))) methodCodes

parsePaths : JsonValue -> Either String (List (String, String, Int))
parsePaths (JsonObject obj) = let tuples = toList obj
                                  nested = traverse parseAll tuples
                                  all = map (\n => n >>= id) nested
                              in all
parsePaths v = Left "Path empty"

toHandlerType : (String,String,Int) -> Type
toHandlerType (path,method,code) = Handler method path code ()

-- JsonObject
parseSwagger : JsonValue -> Either String (List Type)
parseSwagger (JsonObject values) =
  do
    paths <- maybeToEither "No paths found in swagger" $ lookup "paths" values
    triples <- parsePaths paths
    pure $ map toHandlerType triples

parseSwagger x = Left "Invalid swagger root"

ListOfTypesToTuple : List Type -> Type
ListOfTypesToTuple (t1 :: t2 :: __) = (t1, t2)

json : String
json = """{
  "paths": {
    "/pets": {
      "get": {
        "responses": {
          "200": {
          }
        }
      },
      "post": {
        "responses": {
          "201": {
          }
        }
      }
    }

  }
}
"""

jsonValue : JsonValue
jsonValue = JsonObject $ fromList [
    ("paths", 
      JsonObject $ fromList [
        ("/pets",
        JsonObject $ fromList [
          ("get",
          JsonObject $ fromList [
            ("responses",
            JsonObject $ fromList [
              ("201",
              JsonObject $ fromList [])
            ])
          ]),
          ("post",
          JsonObject $ fromList [
            ("responses",
            JsonObject $ fromList [
              ("201",
              JsonObject $ fromList [])
            ])
          ])
        ])
      ])
  ]

loadFile : String -> Either String (Type)
loadFile content = do
  --json <- (Lightyear.Strings.parse jsonToplevelValue) content
  maybeTypes <- parseSwagger Kolgut.Swagger.jsonValue --json
--  ty <- maybeToEither "No types" $ head' maybeTypes
--  pure ty
  pure $ ListOfTypesToTuple maybeTypes

provideTypes : Either FileError String -> Provider (Type)
provideTypes (Left err) = Providers.Error "Some file error"
provideTypes (Right content) = case loadFile content of
                                  Right ty => Provide ty
                                  Left er => Providers.Error er

printFileContents : Either FileError String -> IO ()
printFileContents (Left err) = print "File error"
printFileContents (Right content) = print content


loadSwagger : IO (Provider Type)
loadSwagger = do
            print "Loading file"
            maybeFile <-  readFile "petstore_small.json"
            printFileContents maybeFile
            print "Parsing Json"
            maybeFile <- pure $ Right Kolgut.Swagger.json 
            print "Here"
            pure $ provideTypes maybeFile
