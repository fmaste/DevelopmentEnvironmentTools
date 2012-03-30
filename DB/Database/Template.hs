{-# LANGUAGE TemplateHaskell #-}
module Database.Template (
        -- Export the Template Haskell parts needed to use it.
	Q,
	Dec,
	-- Everything else.
	Template (Template),
	createDb
) where

-------------------------------------------------------------------------------

import Control.Monad
import Data.List (mapAccumL)
import qualified Database as DB
import qualified Data.Map as Map
import Language.Haskell.TH

-------------------------------------------------------------------------------

data Template = Template Databases Tables Fields
type Databases = Map.Map Name DB.Database
type Tables = Map.Map Name DB.Table
type Fields = Map.Map Name DB.Field

zeroTemplate :: Template
zeroTemplate = Template Map.empty Map.empty Map.empty

addDatabase :: DB.Database -> Template -> Template
addDatabase (DB.Database databaseName tables) tp = tp

addTable :: DB.Table -> Template -> Template
addTable (DB.Table tableName fields) tp = tp

addField :: DB.Field -> Template -> Template
addField (DB.Field fieldName fieldType) tp = tp

addFK :: DB.FK -> Template -> Template
addFK _ tp = tp

-------------------------------------------------------------------------------

createDb :: Name -> Q [Dec] -> Q [Dec]
createDb ansName userDecs' = do
	userDecs <- runQ userDecs'
	partialAddExpLists <- mapM addDec userDecs
	let partialAddExps = concat partialAddExpLists
	expAndVarNameTuples <- mapM 
		(\exp -> do
			tmpVarName <- newName "databaseTemplate";
			return (exp, tmpVarName)
		) 
		partialAddExps
	initialTmpVarName <- newName "databaseTemplate"
	let (lastTmpVarName, addDecs) = 
		mapAccumL mapAccumFunction initialTmpVarName expAndVarNameTuples 
	let dbDec = [
		SigD ansName (ConT 'Template),
		FunD ansName [Clause [] (NormalB $ VarE lastTmpVarName) []],
		ValD (VarP initialTmpVarName) (NormalB $ VarE 'zeroTemplate) []
		]
	return (userDecs ++ dbDec ++ addDecs)

mapAccumFunction :: Name -> (Exp, Name) -> (Name, Dec)
mapAccumFunction lastTmpVarName (partialAddExp, tmpVarName) = (tmpVarName, dec) where
	dec = ValD (VarP tmpVarName) (NormalB $ AppE partialAddExp (VarE lastTmpVarName)) []

addDec :: Dec -> Q [Exp]
addDec (ValD (VarP varName) (NormalB exp) []) = addValD varName
addDec _ = return []

addValD :: Name -> Q [Exp]
addValD varName = do
	info <- reify varName
	case info of
		(VarI _ t _ _) -> addVarI varName t
		_ -> return []

addVarI :: Name -> Type -> Q [Exp]
addVarI varName t = do
	let databaseName = ('DB.Database)
	let tableName = ('DB.Table)
	let fieldName = ('DB.Field)
	let fkName = ('DB.FK)
	case t of
		databaseName -> addDatabase' varName
		tableName -> addTable' varName
		fieldName -> addField' varName
		fkName -> addFK' varName
		_ -> return []

addDatabase' :: Name -> Q [Exp]
addDatabase' varName = add' 'addDatabase varName

addTable' :: Name -> Q [Exp]
addTable' varName = add' 'addTable varName

addField' :: Name -> Q [Exp]
addField' varName = add' 'addField varName

addFK' :: Name -> Q [Exp]
addFK' varName = add' 'addFK varName 

-- Return and expression that is the addSomething function partially applied.
-- The expressions are of type (Template -> Template)
add' :: Name -> Name -> Q [Exp]
add' functionName varName = return $ [AppE (VarE functionName) (VarE varName)]

