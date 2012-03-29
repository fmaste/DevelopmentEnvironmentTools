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

addDec :: Dec -> Q [Dec]
addDec (ValD (VarP name) (NormalB exp) []) = addValD name
addDec _ = return []

addValD :: Name -> Q [Dec]
addValD name = do
	info <- reify name
	case info of
		(VarI _ t _ _) -> addVarI name t
		_ -> return []

addVarI :: Name -> Type -> Q [Dec]
addVarI name t = do
	let databaseName = ('DB.Database)
	let tableName = ('DB.Table)
	let fieldName = ('DB.Field)
	let fkName = ('DB.FK)
	case t of
		databaseName -> addDatabase' name
		tableName -> addTable' name
		fieldName -> addField' name
		fkName -> addFK' name
		_ -> return []

addDatabase' :: Name -> Q [Dec]
addDatabase' name = return []

addDatabase :: DB -> DB.Database -> DB
addDatabase (DB.Database databaseName tables) = databaseName

addTable' :: Name -> Q [Dec]
addTable' name = return []

addTable :: DB -> DB.Table -> DB
addTable db (DB.Table tableName fields) = db

addField' :: Name -> Q [Dec]
addField' name = return []

addField :: DB -> DB.Field -> DB
addField db (DB.Field fieldName fieldType) = db

addFK' :: Name -> Q [Dec]
addFK' name = return []

addFK :: DB -> DB.FK -> DB
addFK fk = "FK"

