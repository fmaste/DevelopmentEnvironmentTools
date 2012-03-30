{-# LANGUAGE TemplateHaskell #-}
module Playground (
	main
) where

-------------------------------------------------------------------------------

import Database
import Language.Haskell.TH

-------------------------------------------------------------------------------

database = Database "database" [table]

table = Table "table" []

main :: IO ()
main = do
	putStrLn showPrint
	putStrLn prettyPrint
	putStrLn reification
	putStrLn app
	putStrLn (show $ mkName "function")

showPrint :: String
showPrint = $([d| database = Database "database" [table] |] >>= stringE . show)

prettyPrint :: String
prettyPrint = $([d| database = Database "database" [table] |] >>= stringE . pprint)

reification :: String
reification = $(reify 'database >>= stringE . show)

add :: Int -> Int -> Int
add a b = a + b
	
app :: String
app = $([d| result = add 1 2 |] >>= stringE . show)

