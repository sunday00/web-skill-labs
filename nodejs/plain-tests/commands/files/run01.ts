import { open } from "node:fs/promises"
import { openSync, closeSync } from "node:fs"
import * as console from "node:console";

async function readFile() {
  await using file = await open('./commands/files/test001.txt', 'r')
  //      |
  //      +---- this "using" keyword makes auto close file.
  const text = await file.readFile('utf-8')
  return text
}

readFile().then(console.log)

function readFiles() {
  using stack = new DisposableStack()

  stack.adopt(openSync('./commands/files/test001.txt', 'r'), closeSync)
  stack.adopt(openSync('./commands/files/test001.txt', 'r'), closeSync)
  stack.adopt(openSync('./commands/files/test001.txt', 'r'), closeSync)
  // ...
}

async function readAsyncFiles() {
  await using stack = new AsyncDisposableStack()

  const handles = await Promise.all([
    await open('./commands/files/test001.txt', "r"),
    await open('./commands/files/test001.txt', "r"),
    await open('./commands/files/test001.txt', "r"),
    await open('./commands/files/test001.txt', "r"),
  ])

  return handles.map((handle) => handle.toString())
}