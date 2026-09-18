import { Injectable, MessageEvent } from '@nestjs/common'
import { type Request } from 'express'
import { Subject } from 'rxjs'

@Injectable()
export class FeatService {
  private readonly observer = new Subject<MessageEvent>()

  async index() {
    return {}
  }

  async subscribe() {
    return this.observer.asObservable()
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
}
