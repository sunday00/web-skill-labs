export {}

declare global {
  interface Console {
    process(text: string): void
  }
}

console.process = (text: string) => {
  process.stdout.clearScreenDown()
  // process.stdout.clearLine(0)
  process.stdout.cursorTo(0)
  process.stdout.write(text)
}
