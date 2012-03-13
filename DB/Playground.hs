{-# LANGUAGE TemplateHaskell #-}
module Playground (
	main
) where

-------------------------------------------------------------------------------

import Database
import Language.Haskell.TH

-------------------------------------------------------------------------------

caca = Database "caca" []

main :: IO ()
main = do
	putStrLn play

play :: String
play = $(stringE . show =<< reify 'caca)
	

