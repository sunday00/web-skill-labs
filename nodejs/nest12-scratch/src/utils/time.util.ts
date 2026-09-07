import { Temporal } from 'temporal-polyfill'

export class Time {
  public T: Temporal.ZonedDateTime

  constructor() {
    this.T = Temporal.Now.zonedDateTimeISO()
  }

  diff(other: Temporal.ZonedDateTime) {
    return this.T.until(other)
  }
}
