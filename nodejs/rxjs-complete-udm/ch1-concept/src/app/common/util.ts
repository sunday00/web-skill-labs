import { Observable } from 'rxjs'

export function createHttpObservable<T>(url: string): Observable<T> {
  return new Observable((observer) => {
    const controller = new AbortController()
    const signal = controller.signal

    fetch(url, { signal })
      .then((res) => {
        if (res.ok) {
          return res.json()
        } else {
          return observer.error('failed with : ' + res.status)
        }
      })
      .then((body) => {
        observer.next(body)
        observer.complete()
      })
      .catch((error) => {
        observer.error(error)
      })

    return () => controller.abort()
  })
}
