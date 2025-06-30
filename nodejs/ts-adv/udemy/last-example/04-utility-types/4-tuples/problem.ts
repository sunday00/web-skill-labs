type Arr<T> = [T, ...Array<T>]

const processArray = (input: Arr<string>) => {}

processArray(['bla'])
processArray(['bla', 'blabla', 'blablabla'])

//This should error
// processArray([])

export {}
