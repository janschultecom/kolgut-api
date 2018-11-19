module Main

import Kolgut.Swagger

%language TypeProviders

%provide (Api : Type) with loadSwagger

x : Api
x = (Handle (\c => pure NoContent), Handle (\c => pure NoContent) )

runHandler : Handler m p s a -> String -> String -> a -> Maybe (IO (Response s))
runHandler (Handle f) method path x = if m == method  && path == p then (Just $ f x) else Nothing


getStatus : Response a -> String
getStatus {a} _ = show a

getMsg : Response a -> String
getMsg (Ok r) = r
getMsg NoContent = "Nothing here"
getMsg NotFound = "Not Found"
getMsg InternalServerError = "Server Error"

toStr : Response a -> (String, String)
toStr r = ("Status: " ++ getStatus r ++ "\n", getMsg r)

route : Api -> String -> String -> IO (String, String)
route (h1, h2) m p = let
           result1 = (toStr <$>) <$> runHandler h1 m p ()
           result2 = (toStr <$>) <$> runHandler h2 m p ()
           in maybe (pure $ toStr NotFound) id (result1 <+> result2)

getPets : () -> IO (Response 201)
getPets () = pure NoContent 

getPets2 : () -> IO (Response 200) 
getPets2 () = pure $ Ok "Sooty\nSmoky\n" 

postPets : () -> IO (Response 201)
postPets () = pure NoContent

api : Api
api = (Handle getPets, Handle postPets) -- WILL COMPILE
--api = (Handle getPets2, Handle postPets) -- WILL NOT COMPILE 

main : IO ()
main = do
     l <- getLine
     let [method, path] = words l
     (headers, body) <- route api method path
     putStrLn $ "Content-type: text/html\n" ++ headers ++ "\n";
     putStrLn body
