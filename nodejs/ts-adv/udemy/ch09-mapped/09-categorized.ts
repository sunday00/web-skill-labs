const elements = [
  { category: 'photo', file: 'image1.jpg' },
  { category: 'photo', file: 'image2.jpg' },
  { category: 'photo', file: 'image3.jpg' },
  { category: 'note', file: 'hello world' },
  { category: 'note', file: 'welcome to the world' },
]

const categorize = <T extends { category: string }>(elements: T[]) => {
  const temp: { [K in keyof T as T['category']]?: T[] } = {}

  elements.forEach((el: T) => {
    if (temp.hasOwnProperty(el['category']))
      (temp[el['category'] as T['category']] as T[]).push(el as T)
    else
      temp[el['category'] as T['category']] = [el] as {
        [K in keyof T as T['category']]?: T[]
      }[T['category']]
  })

  return temp
}

console.log(categorize(elements))

type Categorize<T extends { category: string }> = {
  [Obj in T as Obj['category']]?: Obj[]
}

const categorize2 = <T extends { category: string }>(elements: T[]): Categorize<T> => {
  const temp: Categorize<T> = {}

  elements.forEach((el: T) => {
    if (temp.hasOwnProperty(el['category'])) (temp[el['category'] as T['category']] as T[]).push(el)
    else temp[el['category'] as T['category']] = [el] as Categorize<T>[T['category']]
  })

  return temp
}

console.log(categorize2<(typeof elements)[number]>(elements))
