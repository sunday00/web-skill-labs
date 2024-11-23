import { init } from 'my-package'

class Block {
  constructor(private data: string) {}

  static Hello() {
    return 'hi!!'
  }
}

console.log(Block.Hello())

init()
