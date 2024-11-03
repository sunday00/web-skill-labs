type TemplateLiteralKeys = `${'id' | 'title' | 'author'}`

// type ObjWithKeys = any; //{ID: string, TITLE: string, AUTHOR: string}

// type ObjWithKeys = {
//   [K in Uppercase<TemplateLiteralKeys>]: string
// }

type ObjWithKeys = Record<Uppercase<TemplateLiteralKeys>, string>

export {}
