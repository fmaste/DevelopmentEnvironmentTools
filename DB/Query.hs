{-# LANGUAGE TemplateHaskell #-}
module Query (
	createFunctionFromTableWhere
) where

-------------------------------------------------------------------------------

-- Import Template Haskell interfaces
import Data.Maybe
import Control.Monad
import Database
import Language.Haskell.TH

-------------------------------------------------------------------------------

createFunctionFromTableWhere :: Name -> (Database, Table) -> [(Field, Ordering)] -> Q [Dec]
createFunctionFromTableWhere funName (Database dbName _, Table tableName _) fs = do
	parametersNames <- mapM (parameterName . fst) fs
	(fts, ords) <- return $ unzip fs
	forBody <- return $ zip3 parametersNames fts ords
	return 
		[
			SigD funName $ functionType (map (parameterType . fst) fs) ''String,
			FunD funName [Clause (map VarP parametersNames)] (functionBody tableName forBody) [] 
		]


parameterType :: Field -> Type
parameterType (FieldType ValueBool) = ConT ''Bool 
parameterType (FieldType ValueInt) = ConT ''Int
parameterType (FieldType ValueString) = ConT ''String
parameterType (MaybeFieldType valueType) = AppT (ConT ''Maybe) (parameterType $ FieldType valueType)

-- f :: Int -> String
-- (AppT (AppT ArrowT (ConT Int)) (ConT String)
-- g :: Int -> String -> Bool
-- (AppT (AppT ArrowT (ConT Int)) (AppT (AppT ArrowT (ConT String)) (ConT Bool)))
functionType :: [Type] -> Type -> Type
functionType [] result = result
functionType (t:ts) result = AppT (AppT ArrowT t) (functionType ts result)

parameterName :: Type -> Q Name
parameterName (FieldType ValueBool) = do
	name <- newName "param" 
	return name
parameterPattern (MaybeFieldType valueType) = parameterPattern (FieldType valueType)

functionBody :: String -> [(Name, Field, Ordering)] -> Q Exp
functionBody tableName _ = 
	[|
		(queryPrefix tableName) ++ ";"
	|]

queryPrefix :: String -> String
queryPrefix tableName = "SELECT * FROM " ++ tableName


test :: Database -> IO ()
test db = do
	code <- runQ $ [| db |]
	putStrLn (pprint code)
	putStrLn (show code)
	return ()

