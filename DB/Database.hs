module Database (
	Permission,
	ServerHost,
	Server (Server),
	User,
	Password,
	ServerDatabase (ServerDatabase),
        DatabaseName,
        Database (Database),
        TableName,
        Table (Table),
        FieldName,
        Field (Field),
	Nullable (Null, NotNull),
        FieldType (FieldType),
        ValueType (ValueBool, ValueInt, ValueString)
) where

-------------------------------------------------------------------------------

-- Read-write or read-only.
data Permission = RW | RO

-- A server has a host and databases associated.
type ServerHost = String

data Server = Server ServerHost [ServerDatabase]

-- The server databases have a user and a password.
type User = String

type Password = String

data ServerDatabase = ServerDatabase Database User Password

-- The database has a name and tables.
type DatabaseName = String

data Database = Database DatabaseName [Table]

-- Every table has a name a fields.
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

