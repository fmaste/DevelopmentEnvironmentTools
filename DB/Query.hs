{-# LANGUAGE TemplateHaskell #-}
module Query (
	createFunctionFromTableWhere
) where

-------------------------------------------------------------------------------

import Data.Maybe
import Control.Monad
import Database
import Language.Haskell.TH

-------------------------------------------------------------------------------

createFunctionFromTableWhere :: Name -> (Database, Table) -> [(Field, Ordering)] -> Q [Dec]
createFunctionFromTableWhere funName (Database dbName _, Table tableName _) fos = do
	let (fields, ords) = unzip fos
	paramsNames <- mapM parameterName fields
	let patterns = map VarP paramsNames
	let paramsTypes = map parameterType fields
	let funType = functionType paramsTypes (ConT ''String)
	funBody <- functionBody tableName $ zip3 paramsNames fields ords
	return 
		[
			functionSignature funName funType,
			FunD funName [Clause patterns funBody []] 
		]

parameterName :: Field -> Q Name
parameterName (Field n _) = do
	name <- newName n
	return name

parameterType :: Field -> Type
parameterType (Field n (FieldType Null ValueBool)) = ConT ''Bool 
parameterType (Field n (FieldType Null ValueInt)) = ConT ''Int
parameterType (Field n (FieldType Null ValueString)) = ConT ''String
parameterType (Field n (FieldType NotNull valueType)) = 
	AppT (ConT ''Maybe) (parameterType $ Field n $ FieldType Null valueType)

functionSignature :: Name -> Type -> Dec
functionSignature n t = SigD n t

-- f :: Int -> String
-- (AppT (AppT ArrowT (ConT Int)) (ConT String)
-- g :: Int -> String -> Bool
-- (AppT (AppT ArrowT (ConT Int)) (AppT (AppT ArrowT (ConT String)) (ConT Bool)))
functionType :: [Type] -> Type -> Type
functionType [] result = result
functionType (t:ts) result = AppT (AppT ArrowT t) (functionType ts result)

functionBody :: String -> [(Name, Field, Ordering)] -> Q Body
functionBody tableName _ = do
	code <- [|
			(queryPrefix tableName) ++ ";"
		|]
	return $ NormalB code

queryPrefix :: String -> String
queryPrefix tableName = "SELECT * FROM " ++ tableName

