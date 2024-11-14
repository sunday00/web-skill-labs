import { Observable } from 'rxjs'
import { tap } from 'rxjs/operators'

export enum RxJsLoggingLevel {
  TRACE,
  DEBUG,
  INFO,
  ERROR,
}

let rxjsLogging = RxJsLoggingLevel.INFO

export function setRxJsLoggingLevel(level: RxJsLoggingLevel) {
  rxjsLogging = level
}

export const debug = (level: number, message: string) => {
  return (source: Observable<any>) => {
    return source.pipe(
      tap((val) => {
        // any custom logic

        if (level >= rxjsLogging) {
          console.log(message + ' : ', val)
        }
      }),
    )
  }
}
