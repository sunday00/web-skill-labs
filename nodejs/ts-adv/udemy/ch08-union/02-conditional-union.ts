type IsNumber<T> = T extends number ? true : false
type Test1 = IsNumber<1>
type Test2 = IsNumber<'ds'>
type Test3 = IsNumber<1 | 'ds' | 2>

type SnakeToCamel<S> = S extends `${infer H}_${infer T}` ? `${H}${SnakeToCamel<Capitalize<T>>}` : S
type Stc1 = SnakeToCamel<'ho_ho_ho'>
type Stc2 = SnakeToCamel<'russia_athletic_player'>
type Stc3 = SnakeToCamel<'holy'>
type Stc4 = SnakeToCamel<'ho_ho_ho' | 'academy' | 'mongo_db_host'>
