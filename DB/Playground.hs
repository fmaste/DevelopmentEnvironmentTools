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

showPrint :: String
showPrint = $([d| database = Database "database" [table] |] >>= stringE . show)

prettyPrint :: String
prettyPrint = $([d| database = Database "database" [table] |] >>= stringE . pprint)

reification :: String
reification = $(reify 'database >>= stringE . show)
	

