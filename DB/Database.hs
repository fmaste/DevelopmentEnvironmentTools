module Database (
        DatabaseName,
        Database (Database),
        TableName,
        Table (Table),
        FieldName,
        Field (Field),
        FieldType (FieldType, MaybeFieldType),
        ValueType (ValueBool, ValueInt, ValueString)
) where

-------------------------------------------------------------------------------

type DatabaseName = String

data Database = Database DatabaseName [Table]

type TableName = String

data Table = Table TableName [Field]

type FieldName = String

data Field = Field FieldName FieldType

data FieldType = FieldType ValueType | MaybeFieldType ValueType

data ValueType = ValueBool | ValueInt | ValueString

