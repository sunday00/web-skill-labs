import {Temporal} from "temporal-polyfill";

const s = ["2026-08-26", "2024-08-26", "2026-04-26", "2023-12-26", "2030-01-01"]
s.sort(Temporal.PlainDate.compare)
console.log(s)

const today = Temporal.PlainDateTime.from("2026-08-26 18:24:44")
const diff = today.until("2027-08-26 18:25:13")
console.log(diff.days)

const trimSmall = today.round('hours')
console.log(trimSmall.toString())

