type KVSplitter<T extends string> = T extends `${infer K}:${infer V}` ? { k: K; v: V } : T

type A = KVSplitter<'name:sunday00'>

const nameObj: A = { k: 'name', v: 'sunday00' }

export function run31() {
  console.log(nameObj)
}
