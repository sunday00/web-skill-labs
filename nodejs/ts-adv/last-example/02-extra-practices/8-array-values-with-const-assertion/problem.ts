const languages = ['java', 'c', 'go'] as const

// type JavaOrGo = (typeof languages)[0 | 2] // "java" | "go"
// type Languages = (typeof languages)[keyof typeof languages]
type Languages = (typeof languages)[number]

const l: Languages = 'java'

export {}

type JavaOrGo = Exclude<Languages, 'c'>
