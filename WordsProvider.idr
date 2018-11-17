module WordsProvider

import Data.String

-- split lines
readLines : String -> List String
readLines content = let words = Strings.split (\c => List.elem c ['\n', '\r']) content in
                        filter (/= "") words

export
readWords : String -> IO (Provider (List String))
readWords fileName = do maybeFile <- readFile fileName
                        pure $ case maybeFile of 
                                       Left error => Error $ "Failed to load seo words: " ++ show error
                                       Right content => Provide (readLines content) 

export
readType : String -> IO (Provider Type)
readType fileName = do _ <- putStrLn "reading type"
                       pure (Provide Int)
