import { Temporal } from "temporal-polyfill"

const dt = "2026-08-26T16:58:31.438"

const dd1 = Temporal.Duration.from({ hours: 1 })
console.log(dd1.toString())

const t01 = Temporal.ZonedDateTime.from("2026-08-26T17:32:49.880[Asia/Seoul]")
const t02 = t01.add(dd1).withTimeZone("Europe/Berlin")
console.log(t02.toString()) // 17 + 1 - 7

const t03 = Temporal.ZonedDateTime.from("2026-08-26T17:32:49.880[Asia/Seoul]")
const t04 = t03.withTimeZone("Europe/Berlin").getTimeZoneTransition("next") // next DST(Daylight Saving Time, AKA summerTime) change date.
console.log(t04?.toString())

// temporal 을 사용하면 섬머타임이 적용되는 국가, 시점에 통과하면 자동으로 해당 시간까지 반영해서 계산한다.

const t05 = Temporal.PlainDate.from("2026-08-26")
const t06 = t05.add({ days: 1 })
console.log(t05.toString(), t06.toString())