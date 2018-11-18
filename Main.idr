module Main

import Kolgut.Swagger


main : IO ()
main = do
          maybeTypes <- loadSwagger
          case maybeTypes of
            Left error => putStrLn error
            Right types => putStrLn ""

