import crypto from 'node:crypto'

interface BlockShape {
  hash: string
  prevHash: string
  height: number
  data: string
}

class Block implements BlockShape {
  public hash: string
  constructor(
    public prevHash: string,
    public height: number,
    public data: string,
  ) {
    this.hash = Block.calculateHash(prevHash, height, data)
  }

  public static calculateHash(
    prevHash: string,
    height: number,
    data: string,
  ): string {
    return crypto
      .createHash('sha-256')
      .update(`${prevHash}${height}${data}`)
      .digest('hex')
  }
}

class BlockChain {
  private blocks: Block[] = []
  constructor() {
    this.blocks = []
  }

  public addBlock(data: string): void {
    const block = new Block(this.getPrevHash(), this.blocks.length + 1, data)
    this.blocks.push(block)
  }

  public getBlocks() {
    return [...this.blocks]
  }

  private getPrevHash(): string {
    if (this.blocks.length == 0) return ''
    return this.blocks[this.blocks.length - 1].hash
  }
}

const blockChain = new BlockChain()
blockChain.addBlock('first one')
blockChain.addBlock('second one')
blockChain.addBlock('3rd one')
blockChain.addBlock('4 one')

console.log(blockChain.getBlocks())
