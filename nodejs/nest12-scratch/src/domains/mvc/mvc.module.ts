import { Module } from '@nestjs/common'
import { MvcController } from './mvc.controller.js'
import { MvcService } from './mvc.service.js'
import { KillDragonHandler } from './cqrs/kill.dragon.q.js'
import { MvcRepository } from './mvc.repository.js'
import { HeroKilledDragonHandler } from './cqrs/kill.dragon.e.js'
import { HeroHandler } from './cqrs/hero.q.js'

@Module({
  controllers: [MvcController],
  providers: [
    MvcService,

    KillDragonHandler,
    HeroKilledDragonHandler,
    HeroHandler,

    MvcRepository,
  ],
})
export class MVCModule {}
