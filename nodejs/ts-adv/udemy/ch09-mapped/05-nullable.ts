type NullableValues<T> = {
  [Key in keyof T]: T[Key] | null
}

type NullableArrays = NullableValues<string[]>
type NullableTuples = NullableValues<[1, string, boolean, null]>

type OptionalArr = Partial<boolean[]>
type OptionalTup = Partial<[boolean, string]>

const RA: Required<boolean[]> = []
const OA: OptionalArr = []
const RT: Required<[boolean, string]> = [true, '1']
const OT: OptionalTup = [true]

type ReRequired = Required<{ name?: string }>
const RR: ReRequired = { name: '' }
