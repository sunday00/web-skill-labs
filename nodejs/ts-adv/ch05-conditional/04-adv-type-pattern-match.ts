type IsProduct<T> = T extends { title: string; price: number } ? true : false

type YesProduct = IsProduct<{ title: string; price: number }>
type NoProduct = IsProduct<{ title: string }>

type Plan = 'pro' | 'premium' | 'starter'
type Role = 'reader' | 'editor' | 'admin'
type CanEdit<P extends Plan, R extends Role> = [P, R] extends [
  'pro' | 'premium',
  'editor' | 'admin',
]
  ? true
  : false
