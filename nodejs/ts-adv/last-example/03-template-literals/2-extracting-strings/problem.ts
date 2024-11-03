type Tools = 'Core Java and Spring Boot' | 'Golang' | 'Node, MongoDB and React'

// type MultipleTools = Exclude<Tools, 'Golang'>
type MultipleTools = Extract<Tools, `${string} and ${string}`>

export {}
