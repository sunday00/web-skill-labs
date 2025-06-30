type Splittable<T> = T extends `${infer A}-${infer B}` ? [A, B, '-'] : never

type ZipCode = Splittable<`${number}-${number}`>
type ZipCode2 = Splittable<'123-456'>

const toArray = (splittable: string) => {
  const split = splittable.split('-')
  split.push('-')
  return split as ZipCode
}
const zipCode: ZipCode = toArray('000-111')

console.log('zipCode', zipCode)

type MultipleSplit<S, ACC = string[]> = ACC extends [...infer A, infer B]
  ? B extends `${infer H}-${infer T}`
    ? MultipleSplit<S, [...A, H, T]>
    : [...A, B]
  : S extends `${infer H}-${infer T}`
    ? MultipleSplit<S, [H, T]>
    : S
type KebobElementArray = MultipleSplit<'abc-def-fff-lll'>
