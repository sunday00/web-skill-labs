import { IQuery, IQueryHandler, QueryHandler } from '@nestjs/cqrs'
import {
  EventEmitter2,
  EventEmitterReadinessWatcher,
} from '@nestjs/event-emitter'

export class EventWildTrigger implements IQuery {
  constructor(public name: string) {}
}

@QueryHandler(EventWildTrigger)
export class EventWildHandler implements IQueryHandler<EventWildTrigger> {
  constructor(
    private emitter: EventEmitter2,
    private guaranty: EventEmitterReadinessWatcher,
  ) {}

  async execute(query: EventWildTrigger): Promise<any> {
    await this.guaranty.waitUntilReady()

    const r = await this.emitter.emitAsync('from.wild.*', { name: query.name })

    return r
  }
}
