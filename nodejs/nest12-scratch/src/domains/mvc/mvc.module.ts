import { Module } from '@nestjs/common'
import { MvcController } from './mvc.controller.js'
import { MvcService } from './mvc.service.js'
import { KillDragonHandler } from './cqrs/kill.dragon.q.js'
import { MvcRepository } from './mvc.repository.js'
import { HeroKilledDragonHandler } from './cqrs/kill.dragon.e.js'
import { HeroHandler } from './cqrs/hero.q.js'
import { SagaMain } from './cqrs/saga/saga.trigger.js'
import { SagaStep1Handler } from './cqrs/saga/saga.step1.js'
import { SagaStep2Handler } from './cqrs/saga/saga.step2.js'
import { SagaStep4Handler } from './cqrs/saga/saga.step4.js'
import { SagaStep3Handler } from './cqrs/saga/saga.step3.js'

@Module({
  controllers: [MvcController],
  providers: [
    MvcService,

    KillDragonHandler,
    HeroKilledDragonHandler,
    HeroHandler,

    SagaMain,
    SagaStep1Handler,
    SagaStep2Handler,
    SagaStep3Handler,
    SagaStep4Handler,

    MvcRepository,
  ],
})
export class MVCModule {}
