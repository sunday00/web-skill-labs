import { IQuery, IQueryHandler, QueryHandler } from '@nestjs/cqrs'
import {
  EventEmitter2,
  EventEmitterReadinessWatcher,
} from '@nestjs/event-emitter'

export class EventFireTrigger implements IQuery {
  constructor(public name: string) {}
}

@QueryHandler(EventFireTrigger)
export class EventFireHandler implements IQueryHandler<EventFireTrigger> {
  constructor(
    private emitter: EventEmitter2,
    private guaranty: EventEmitterReadinessWatcher,
  ) {}

  async execute(query: EventFireTrigger): Promise<any> {
    await this.guaranty.waitUntilReady()

    const r = await this.emitter.emitAsync('from.animal', { name: query.name })

    return r
  }
}
