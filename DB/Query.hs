{-# LANGUAGE TemplateHaskell #-}
module Query (
	getFromTableWhere
) where

-------------------------------------------------------------------------------

-- Import Template Haskell interfaces
import Data.Maybe
import Database
import Language.Haskell.TH

-------------------------------------------------------------------------------

createFunctionFromTableWhere :: Name -> (Database, Table) -> [(Field, Ordering)] -> Q [Dec]
createFunctionFromTableWhere funName (Database dbName _, Table tableName _) fs = return
	[
		FunD funName 
	]

-- Return a function that receives a param of some type.
-- And returns a string that queries that part.
-- For example:
-- 	fun :: Int -> String
--	fun num = "NUM = " ++ (show num)
getFunctionForType :: (Field, Ordering) -> Q Exp
getFunctionForType (Field fieldName fieldType, ord) = [| 
		\x -> fieldName ++ " " ++ ($(getFunctionForType2 (fieldType, ord)) x) 
	|]

getFunctionForType2 :: (FieldType, Ordering) -> Q Exp
getFunctionForType2 (FieldType valueType, ord) = [|
		\x -> 

	|]
getFunctionForType2 (MaybeFieldType valueType, ord) = [|
		\x -> if isNothing x 
			then if ord == EQ
				then "IS NULL"
				else if ord == NEQ
					then "IS NOT NULL"
					else (error $ "using " ++ (show ord) ++ " with null.")
			else ($(getFunctionForType2 (FieldType valueType, ord)) x)
	|]

test :: Database -> IO ()
test db = do
	code <- runQ $ [| db |]
	putStrLn (pprint code)
	putStrLn (show code)
	return ()

