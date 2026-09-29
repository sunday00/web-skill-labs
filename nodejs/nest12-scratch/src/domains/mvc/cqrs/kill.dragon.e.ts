import { EventsHandler, IEvent, IEventHandler } from '@nestjs/cqrs'
import { MvcRepository } from '../mvc.repository.js'

export class KillDragonE implements IEvent {
  constructor(public readonly id: string) {}
}

@EventsHandler(KillDragonE)
export class HeroKilledDragonHandler implements IEventHandler<KillDragonE> {
  constructor(private repository: MvcRepository) {}

  handle(event: KillDragonE) {
    console.log(event.id)
  }
}
