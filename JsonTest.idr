module Main

import Kolgut.Swagger
import Kolgut.Json


test : String -> Either String JsonValue
test = Lightyear.Strings.parse jsonToplevelValue

main : IO ()
main = do 
  maybeFile <- readFile "petstore_test.json"
  case maybeFile of 
       Right content => 
         do 
           _ <- putStrLn "Successfully loaded file:"
           _ <- putStrLn content
           case test content of 
              Right json => putStrLn $ show json
              Left error => putStrLn error
       Left error => putStrLn $ show error
