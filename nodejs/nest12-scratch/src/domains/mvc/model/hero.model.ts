import { AggregateRoot } from '@nestjs/cqrs'
import { KillDragonE } from '../cqrs/kill.dragon.e.js'

export class Hero extends AggregateRoot {
  id: string
  name: string

  constructor() {
    super()

    this.autoCommit = true
  }

  action() {
    this.apply(new KillDragonE(this.id))
  }
}
