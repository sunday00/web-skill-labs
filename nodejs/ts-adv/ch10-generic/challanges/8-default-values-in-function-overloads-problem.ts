export const dataObj = {
  key1: 'a',
  key2: 'b',
} as const

function getValue(): (typeof dataObj)['key1']
// no key -> auto returns through guessing argument may be key1

function getValue<K extends keyof typeof dataObj>(key: K): (typeof dataObj)[K]
// some key -> returns by key

function getValue(key: keyof typeof dataObj = 'key1') {
  return dataObj[key]
}
// implements the function type

const a = getValue('key1')
const b = getValue('key2')
const defaultValue = getValue() //a
