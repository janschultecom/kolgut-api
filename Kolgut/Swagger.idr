module Kolgut.Swagger

import Kolgut.Json
import Lightyear.StringFile

rErr : String -> FileError -> String
rErr s f = "Got file error: " ++ s 

pErr : String -> String -> String
pErr s1 s2 = "Got parsing error: \n" ++ s1 ++ s2

export
loadSwagger : IO ()
loadSwagger = do
          maybeJson <- run $ parseFile rErr pErr jsonToplevelValue "petstore.json"
          case maybeJson of
               Right json => putStrLn $ "Got json" ++ (show json)
               Left error => putStrLn $ "Got error" ++ error 
