const values = {
  UUID: 'uuid',
  Int: 23,
  String: 'A String.',
  Boolean: false,
  Obj: {
    key: 'value',
  },
}

type ValuesType = typeof values

type UUIDType = ValuesType['UUID']
type IntType = ValuesType['Int']
type StringType = ValuesType['String']
type BooleanType = ValuesType['Boolean']
type ObjType = ValuesType['Obj']
type KeyType = ValuesType['Obj']['key']

export {}

const a: IntType = 3
