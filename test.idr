module Main

import Data.List
import WordsProvider
import Helper

%language TypeProviders

%provide (SeoWords : Type) with readType "seo-words.txt"



data HttpStatus : Int -> Type where
  Ok : HttpStatus 200 



-- /pets/ -> 200 

{-
routes : String -> SwaggerProvider -- HttpStatus 200 
routes "pets" = Ok -- compile
routes "pets" = NotFound -- does not compile
-}
