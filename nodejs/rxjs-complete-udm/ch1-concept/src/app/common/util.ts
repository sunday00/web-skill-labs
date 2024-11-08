import { Observable } from 'rxjs'

export function createHttpObservable(url: string) {
  return new Observable((observer) => {
    const controller = new AbortController()
    const signal = controller.signal

    fetch(url, { signal })
      .then((res) => res.json())
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
