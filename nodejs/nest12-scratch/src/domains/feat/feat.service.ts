import { Injectable, MessageEvent } from '@nestjs/common'
import { type Request } from 'express'
import { finalize, Subject, tap } from 'rxjs'

@Injectable()
export class FeatService {
  private readonly observer = new Subject<MessageEvent>()

  async index() {
    return {}
  }

  async subscribe(signal: AbortSignal) {
    return this.observer.asObservable().pipe(
      tap(() => console.log(signal)),
      finalize(() => {
        console.log('Client closed')
      }),
    )
  }

  async send(_req: Request, msg: string) {
    // this.observer.next(new MessageEvent(msg))
    this.observer.next({
      id: '1',
      data: { msg },
      type: 'message',
      retry: 1,
    } satisfies MessageEvent)
  }

  async close() {
    return this.observer.complete(
      // finalize(() => {
      //   console.log('close')
      // }),
    )
  }
}
