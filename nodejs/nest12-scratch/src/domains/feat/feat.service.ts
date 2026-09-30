import { Injectable, MessageEvent } from '@nestjs/common'
import { type Request } from 'express'
import { finalize, Subject, tap } from 'rxjs'
import { AsyncLocalStorage } from 'node:async_hooks'
import { ClsService } from 'nestjs-cls'

@Injectable()
export class FeatService {
  private readonly observer = new Subject<MessageEvent>()

  constructor(
    private readonly als: AsyncLocalStorage<any>,
    private readonly cls: ClsService,
  ) {}

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

  async stateOne() {
    console.log(
      this.als.getStore(),
      this.cls.get('from_nestClsModule'),
      this.cls.getId(),
    )

    return 1
  }
}
