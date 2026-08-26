import { Temporal } from "temporal-polyfill"

const dt = "2026-08-26T16:58:31.438"

const t01 = Temporal.PlainDate.from(dt)
console.log(t01, t01.toLocaleString('sv-SE'), t01.toString())

const t02 = Temporal.PlainTime.from(dt)
console.log(t02, t02.toString())

const t03 = Temporal.PlainDateTime.from(dt)
console.log(t03, t03.toString())

const t04 = Temporal.Instant.from(dt + 'Z')
const t04_1 = Temporal.Instant.from(dt + '+09:00')
console.log(t04, t04.toString(), t04_1.toString(), t04_1.toLocaleString('ko-kr'))

