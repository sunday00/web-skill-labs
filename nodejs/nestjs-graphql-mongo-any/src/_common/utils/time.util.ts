import DAYJS, { ConfigType, ManipulateType } from 'dayjs'
import weekdays from 'dayjs/plugin/weekday'
import relativeTime from 'dayjs/plugin/relativeTime'
import updateLocale from 'dayjs/plugin/updateLocale'
import utc from 'dayjs/plugin/utc'
import advancedFormat from 'dayjs/plugin/advancedFormat'
import weekOfYear from 'dayjs/plugin/weekOfYear'
// import 'dayjs/locale/ko'

DAYJS.extend(advancedFormat)
DAYJS.extend(weekdays)
DAYJS.extend(weekOfYear)
DAYJS.extend(updateLocale)
DAYJS.extend(relativeTime)
DAYJS.extend(utc)

export const time = (dateTime?: ConfigType) => {
  if (dateTime)
    return typeof dateTime === 'number'
      ? DAYJS.unix(dateTime)
      : DAYJS(dateTime as string)

  return DAYJS.utc()
}

export const toTimestamp = (value: string) => {
  return time(value).format('YYYY-MM-DD HH:mm:ss')
}

export const week = (date: string) => {
  const point = time(date)

  return `${point.format('M')}M-${Number(point.format('w')) - Number(point.startOf('M').format('w')) + 1}W`
}

export const NOW = () => time()

// MAYBE: need to receive local timezone from frontend client side.
export const LocalTime = (dateTime?: ConfigType) =>
  time(dateTime).add(9, 'hours')

export const TODAY = () => time().format('YYYY-MM-DD 00:00:00')

export const NOW_TIME = () => NOW().format('YYYY-MM-DD HH:mm:ss')

export function sleep(t: number) {
  return new Promise((resolve) => setTimeout(resolve, t))
}

export const toSeconds = (amt: number, unit: ManipulateType) => {
  switch (unit) {
    case 's':
    case 'second':
    case 'seconds':
      return amt
    case 'm':
    case 'minute':
    case 'minutes':
      return amt * 60
    case 'h':
    case 'hour':
    case 'hours':
      return amt * 60 * 60
    case 'd':
    case 'day':
    case 'days':
      return amt * 60 * 60 * 24
    case 'w':
    case 'week':
    case 'weeks':
      return amt * 60 * 60 * 24 * 7
  }
}
