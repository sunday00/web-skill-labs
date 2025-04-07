const { Readable } = require('stream')

class MyStream extends Readable {
  #count = 0
  _read(size) {
    // setImmediate(() => {
      this.push(':-)')
      if (this.#count++ === 5) { this.push(null) }
    // })

  }
}

const stream = new MyStream({
  highWaterMark: 1,
  objectMode: true,
})

stream.on('readable', () => {
  console.count('>> readable event: ')

  let chunk

  while ((chunk = stream.read()) !== null) {
    console.log(chunk.toString())
  }
})

stream.on('end', () => {
  console.info('end of stream')
})