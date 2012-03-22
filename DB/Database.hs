module Database (
	ServerName,
	ServerHost,
	Server (Server),
        DatabaseName,
        Database (Database),
        TableName,
        Table (Table),
        FieldName,
        Field (Field),
	Nullable (Null, NotNull),
        FieldType (FieldType),
        ValueType (ValueBool, ValueInt, ValueString),
	Permission (RW, RO),
	User,
	Password,
	ServerAccess (ServerAccess)
) where

-------------------------------------------------------------------------------

-- A server has a host and databases associated.
type ServerName = String

type ServerHost = String

data Server = Server ServerName ServerHost [Database]

-- The database has a name and tables.
-- This is the name as used for the SQL queries.
type DatabaseName = String

data Database = Database DatabaseName [Table]

-- Every table has a name and fields.
-- This is the name as used for the SQL queries.
type TableName = String

data Table = Table TableName [Field]

-- A field has a name and a type.
type FieldName = String

data Field = Field FieldName FieldType

data Nullable = Null | NotNull

-- The field types are bool, int and string. 
-- Can be not null or nullable.
data FieldType = FieldType Nullable ValueType

data ValueType = ValueBool | ValueInt | ValueString

-- Read-write or read-only.
data Permission = RW | RO

-- The server has a user and a password.
type User = String

type Password = String

data ServerAccess = ServerAccess Server User Password

