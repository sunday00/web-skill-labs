type ContentItem =
  | { category: 'text'; body: string }
  | { category: 'likedBy'; user: string; body: string }
  | { category: 'suggestions'; recommendedUsers: string[] }
  | { category: 'photo'; url: string }
  | { category: 'clip'; url: string }

declare const elements: ContentItem[]

const filterItems1 = (elements: ContentItem[], categories: string[]) =>
  elements.filter((element) => categories.includes(element.category))

const mediaElements = filterItems1(elements, ['photo', 'clip'])

// ---

declare function filterItems<EL extends { category: string }, Category extends EL['category']>(
  array: EL[],
  categories: Category[],
): FilterByCategory<EL, Category>[]

type FilterByCategory<EL, Cat> = EL extends { category: Cat } ? EL : never
type PhotoAndClipped = FilterByCategory<ContentItem, 'photo' | 'clip'>

type FilterByCategoryUnion<EL, Cat> = EL extends Cat ? EL : never
type PhotoAndClipped2 = FilterByCategoryUnion<ContentItem, { category: 'photo' | 'clip' }>

const filterItems2 = (elements: ContentItem[], categories: string[]) =>
  elements.filter((element) => categories.includes(element.category))

const mediaElements2 = filterItems2(elements, ['photo', 'clip'])
