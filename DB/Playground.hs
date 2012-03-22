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
	putStrLn play

play :: String
play = $(stringE . show =<< reify 'database)
	

