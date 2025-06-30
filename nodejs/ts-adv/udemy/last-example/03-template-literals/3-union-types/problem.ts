type FrameWork = 'NodeJS' | 'Spring' | '.Net'
type Database = 'GraphQL' | 'MongoDB' | 'PostgreSQL'

type Backend = `${FrameWork} with ${Database}`
// "NodeJS with GraphQL"    |
// "NodeJS with MongoDB"    |
// "NodeJS with PostgreSQL" |
// "Spring with GraphQL"    |
// ...

const b: Backend = 'NodeJS with GraphQL'

export {}
