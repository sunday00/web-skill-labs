type SuperPrint = {
  <T>(arr: T[]): void
}

const superPrint: SuperPrint = (arr) => {
  arr.forEach((i) => console.log(i))
}

superPrint([1, 2, 3, 4])
superPrint([1, 2, true, false])
superPrint([1, true, false, 'h', 7, { a: '1' }])
