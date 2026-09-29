import { Injectable } from '@nestjs/common'
import { type Response } from 'express'
import { EventBus, EventPublisher, QueryBus } from '@nestjs/cqrs'
import { KillDragon } from './cqrs/kill.dragon.q.js'
import { HeroQ } from './cqrs/hero.q.js'

@Injectable()
export class MvcService {
  constructor(
    private qb: QueryBus,
    private eb: EventBus,
    private publisher: EventPublisher,
  ) {}

  async index() {
    return { message: 'hello' }
  }

  async responseWithView(res: Response, view: string) {
    return res.render(`mvc/${view}`, { name: 'kim' })
  }

  async cqrsQ() {
    const r = await this.qb.execute(new KillDragon('some'))

    return r
  }

  async cqrsE() {
    const hero = this.publisher.mergeObjectContext(
      await this.qb.execute(new HeroQ('1')),
    )
    const hero2 = await this.qb.execute(new HeroQ('2'))
    hero2['publish'] = this.eb.publish

    hero.action()
    hero2.action()

    // hero.commit()
    // hero2.commit()

    return Promise.resolve(undefined)
  }
}
