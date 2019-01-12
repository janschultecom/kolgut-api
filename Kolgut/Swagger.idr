module Kolgut.Swagger

import Kolgut.Json
import Data.String
import Debug.Error

%access public export
-- %language ElabReflection

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
          "200": null
        }
      },
      "post": {
        "responses": {
          "201": null
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
  json <- (Lightyear.Strings.parse jsonToplevelValue) content
  maybeTypes <- parseSwagger json -- Kolgut.Swagger.jsonValue
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

parseSwaggerJson : String -> IO (Either String JsonValue)
parseSwaggerJson content = 
  do
    putStrLn "Starting to parse content:"
    --let json = parse jsonToplevelValue content 
    let jsonStr = "{\"paths\":null}\n" 
    putStrLn jsonStr
    putStrLn $ show $ unpack jsonStr
    let json = Lightyear.Strings.parse jsonToplevelValue content --jsonStr
    --let json = Right Kolgut.Swagger.jsonValue
    putStrLn $ content
    putStrLn $ show $ unpack content
    putStrLn $ "Equal? " ++ show (jsonStr == content)
    putStrLn $ "Length? " ++ (show $ length jsonStr) ++ " " ++ (show $ length content)
    putStrLn $ case json of
                   Right v => "RIGHT"
                   Left e => "LEFT"
    putStrLn "Finished show"
    pure json


test : Either String Type
test = Right Int --Left "Got Left"

loadSwagger2 : IO (Provider Type)
loadSwagger2 = case Kolgut.Swagger.test of
                    Right ty => pure $ Provide ty
                    Left err => pure $ Error err

loadSwagger : IO (Provider Type)
loadSwagger = do
            putStrLn "Loading file"
            maybeFile <-  readFile "petstore_test.json"
            case maybeFile of
                 Right file =>
                   do
                     putStrLn "Successfully loaded file"
                     putStrLn file
                     putStrLn "Parsing file"
--                     pure $ Error "Bla"
                     maybeContent <- parseSwaggerJson file
                     case maybeContent of
                          Right json =>
                              do
                                putStrLn "Successfully parsed json"
                                putStrLn $ show json 

                                case parseSwagger json of
                                  Right swagger =>
                                    do 
                                      putStrLn "Successfully parsed swagger"
                                      pure $ Provide $ ListOfTypesToTuple swagger
                                  Left error => pure $ Error error
                          Left error => 
                            do 
                              putStrLn "Failed parsing json"
                              putStrLn $ show error
                              pure $ Error $ show error
                              
                 Left error => pure $ Error $ show error
