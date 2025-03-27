import DayJs from 'dayjs'
import weekdays from 'dayjs/plugin/weekday'
import relativeTime from 'dayjs/plugin/relativeTime'
import updateLocale from 'dayjs/plugin/updateLocale'
import 'dayjs/locale/ko'

DayJs.extend(weekdays)
DayJs.extend(updateLocale)
DayJs.extend(relativeTime)

DayJs.updateLocale('ko', {
  relativeTime: {
    future: '%s 후',
    past: '%s 전',
    s: '방금',
    m: '1분',
    mm: '%d분',
    h: '1시간',
    hh: '%d시간',
    d: '하루',
    dd: '%d일',
    M: '한달',
    MM: '%d달',
    y: '1년',
    yy: '%d년',
  },
})
DayJs.locale('ko')

export const time = (dateTime?: string | Date | DayJs.Dayjs) => {
  if (dateTime) return DayJs(dateTime)

  return DayJs()
}

export const TIME_FORMAT = {
  DT: 'YYYY-MM-DD HH:mm:ss',
  D: 'YYYY-MM-DD',
  T: 'HH:mm:ss',
} as const
