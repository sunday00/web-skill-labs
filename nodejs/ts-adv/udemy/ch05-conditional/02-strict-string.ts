const returningName = <S>(name: S) => ({ name })
const nameCh5 = returningName('sunday') // type string
// this means, It's ok returning any string even though not sunday.
// this is not what we want.

const returningNameStrict = <S extends string>(name: S) => ({ name })
const nameCh5_2 = returningNameStrict('sunday00')
