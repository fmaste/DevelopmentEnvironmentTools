{-# LANGUAGE TemplateHaskell #-}
module Database.Template (
        -- Export what is needed to use it.
	Q,
	Dec,
	-- Everything else.
	DB (DB),
	createDb
) where

-------------------------------------------------------------------------------

import Control.Monad
import qualified Database as DB
import qualified Data.Map as Map
import Language.Haskell.TH

-------------------------------------------------------------------------------

data DB = DB Databases Tables Fields
type Databases = Map.Map Name DB.Database
type Tables = Map.Map Name DB.Table
type Fields = Map.Map Name DB.Field

createDb :: Name -> Q [Dec] -> Q [Dec]
createDb name decs' = do
	decs <- runQ decs'
	--db <- foldM addDecToDb (DB Map.empty Map.empty Map.empty) decs
	let dbDec = [
		--SigD name (ConT 'DB),
		--FunD name [Clause [] (NormalB $ LitE 1) []]
		]
	return (dbDec ++ decs)

addDec :: DB -> Dec -> Q DB
addDec db (ValD (VarP name) (NormalB exp) []) = addValDToDb db name exp
addDec db _ = return db

addValDToDb :: DB -> Name -> Exp -> Q DB
addValDToDb db name exp = do
	info <- reify name
	case info of
		(VarI _ t _ _) -> addVarIToDb db name exp t
		_ -> return db

addVarIToDb :: DB -> Name -> Exp -> Type -> Q DB
addVarIToDb db name exp' t = do
	let exp = return exp'
	let databaseName = ('DB.Database)
	let tableName = ('DB.Table)
	let fieldName = ('DB.Field)
	let fkName = ('DB.FK)
	case t of
		databaseName -> return $ addDatabaseToDb db name $exp
		tableName -> return $ addTableToDb db name $exp
		fieldName -> return $ addFieldToDb db name $exp
		fkName -> return $ addFKToDb db name $exp

addDatabaseToDb :: DB -> Name -> DB.Database -> DB
addDatabaseToDb db name _ = db

addTableToDb :: DB -> Name -> DB.Table -> DB
addTableToDb db name _ = db

addFieldToDb :: DB -> Name -> DB.Field -> DB
addFieldToDb db name _ = db

addFKToDb :: DB -> Name -> DB.FK -> DB
addFKToDb db name _ = db

