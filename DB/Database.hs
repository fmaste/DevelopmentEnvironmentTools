{-# LANGUAGE TemplateHaskell #-}
module Database (
	-- Export what is needed to use it.
	Q,
	Dec,
	-- The data structure only types.
        DatabaseName,
        Database (Database),
        TableName,
        Table (Table),
        FieldName,
        Field (Field),
        FieldType (FieldType),
	Nullable (Null, NotNull),
        ValueType (ValueBool, ValueInt, ValueString),
	FK (FK),
	ServerName,
	ServerHost,
	Server (Server),
	Permission (RW, RO),
	User,
	Password,
	ServerAccess (ServerAccess)
) where

-------------------------------------------------------------------------------

import Language.Haskell.TH
import qualified Data.Map as Map

-- Data structure.
-------------------------------------------------------------------------------

-- The database has a name and tables.
-- This is the name as used for the SQL queries.
type DatabaseName = String

data Database = Database DatabaseName [Table]
	deriving (Show, Read, Eq, Ord)

-- Every table has a name and fields.
-- This is the name as used for the SQL queries.
type TableName = String

data Table = Table TableName [Field]
	deriving (Show, Read, Eq, Ord)

-- A field has a name and a type.
-- This is the name as used for the SQL queries.
type FieldName = String

data Field = Field FieldName FieldType
	deriving (Show, Read, Eq, Ord)

-- The field types are bool, int and string. 
-- Can be not null or nullable.
data FieldType = FieldType Nullable ValueType
	deriving (Show, Read, Eq, Ord)

data Nullable = Null | NotNull
	deriving (Show, Read, Eq, Ord)

data ValueType = ValueBool | ValueInt | ValueString
	deriving (Show, Read, Eq, Ord)

-- A foreign key is a relation between two fields.
data FK = FK Field Field
	deriving (Show, Read, Eq, Ord)

-------------------------------------------------------------------------------

-- A server has a host and databases associated.
type ServerName = String

type ServerHost = String

data Server = Server ServerName ServerHost [Database]

-- Read-write or read-only.
data Permission = RW | RO

-- The server has a user and a password.
type User = String

type Password = String

data ServerAccess = ServerAccess Server User Password

-- Data Structure Description
-------------------------------------------------------------------------------

data DB = DB Databases Tables Fields
type Databases = Map.Map Name Database
type Tables = Map.Map Name Table
type Fields = Map.Map Name Field

{-
createDb :: Name -> Q [Dec] -> Q [Dec]
createDb name decs' = do
	decs <- runQ decs'
	let db = foldl addToDb (DB Map.empty Map.empty Map.empty) decs
	let dbDec = [
		SigD name (ConT 'DB),
		FunD name [Clause [] (NormalB dbBody) []]
		]
	return (dbDec:decs)

addToDb :: DB -> Dec -> DB
addToDb db (FunD name clauses) = db
addToDb db _ = db
-}

