type Column = {
  name: string
  values: unknown[]
}

type Table = [Column, ...Column[]]

type ClientTable = [
  { name: 'username'; values: string[] },
  { name: 'country'; values: string[] },
  { name: 'plan'; values: string[] },
]

type ExtractColumn<List, Name> = List extends [infer First, ...infer Rest]
  ? First extends { name: Name; values: infer Values }
    ? Values
    : ExtractColumn<Rest, Name>
  : undefined

declare function extractColumn<T extends Table, N extends string>(
  table: T,
  columnName: N,
): ExtractColumn<T, N>

const clients: ClientTable = [
  { name: 'username', values: ['Jack', 'John', 'Jill'] },
  { name: 'country', values: ['USA', 'UK', 'France'] },
  { name: 'plan', values: ['basic', 'basic', 'pro'] },
]

const usernames = extractColumn(clients, 'username')
const plans = extractColumn(clients, 'plan')
console.log(usernames, plans)

enum PostStatus {
  ACTIVE = 'ACTIVE',
  DRAFT = 'DRAFT',
}

declare const posts: [{ name: 'title'; values: string[] }, { name: 'status'; values: PostStatus[] }]
posts[0].values.push('k')

const postsStatus = extractColumn(posts, 'status')
const bla = extractColumn(posts, 'blah blah')
