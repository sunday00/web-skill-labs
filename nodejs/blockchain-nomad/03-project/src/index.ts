import { init } from 'my-package'
import { hello } from './normal'

class Block {
  constructor(private data: string) {}

  static Hello() {
    return 'hi!!'
  }
}

console.log(Block.Hello())

init()
hello('123')
