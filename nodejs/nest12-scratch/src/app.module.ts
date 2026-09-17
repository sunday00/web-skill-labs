import {
  MiddlewareConsumer,
  Module,
  NestModule,
  RequestMethod,
} from '@nestjs/common'
import { AppController } from './app.controller.js'
import { AppService } from './app.service.js'
import { AnimalModule } from './domains/animal/animal.module.js'
import { CqrsModule } from '@nestjs/cqrs'
import { NestedModule } from './domains/nested/nested.module.js'
import {
  ConsoleFunctionMiddleware,
  ConsoleMiddleware,
  UselessFunctionMiddlewareFactory,
} from './aop/middlewares/console.middleware.js'
import { AnimalController } from './domains/animal/animal.controller.js'
import { NestedController } from './domains/nested/nested.controller.js'
import { createObserveModule } from '@nestjs/observe'
import { ScopedModule } from './domains/scoped/scoped.module.js'
import { QueueModule } from './domains/queue/queue.module.js'
import { RedisModule } from './modules/redis/redis.module.js'
import { ConfigModule } from '@nestjs/config'
import RedisConfig from './configs/redis.config.js'
import { QueueBullModule } from './domains/queue/queue.bull.module.js'
import { EventEmitterModule } from '@nestjs/event-emitter'
import { MVCModule } from './domains/mvc/mvc.module.js'

export const { ObserveModule, ObserveInstrument } = createObserveModule()

@Module({
  imports: [
    ConfigModule.forRoot({ isGlobal: true, load: [RedisConfig] }),
    ObserveModule.forRoot({
      appKey: process.env.OBSERVE_APP_KEY ?? '',
      appSecret: process.env.OBSERVE_APP_SECRET ?? '',
      serviceId: 'n12-scratch-33159',
      serviceVersion: 'v1.0.0',

      // ----

      http: {
        getUserId: (req) => req.user?.id ?? 'anonymous',
      },
      runtimeMetrics: true,
      runtimeMetricsInterval: 60000,
      forwardLogs: true,
      redaction: {
        enabled: true,
        useDefaultPatterns: true,
      },
    }),
    RedisModule,
    CqrsModule.forRoot(),
    EventEmitterModule.forRoot({
      wildcard: true,
      delimiter: '.',
      newListener: true,
      removeListener: true,
      maxListeners: 3,
      verboseMemoryLeak: true,
    }),
    AnimalModule,
    NestedModule,
    ScopedModule,
    QueueModule,
    QueueBullModule,
    MVCModule,
  ],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule implements NestModule {
  configure(consumer: MiddlewareConsumer) {
    consumer
      .apply(ConsoleMiddleware)
      // .forRoutes('animal')
      .forRoutes(AnimalController)
      .apply(ConsoleFunctionMiddleware)
      .forRoutes(NestedController)
      .apply(
        UselessFunctionMiddlewareFactory('apple'),
        UselessFunctionMiddlewareFactory('banana'),
        UselessFunctionMiddlewareFactory('cherry'),
      )
      .forRoutes({ path: 'shared', method: RequestMethod.GET })
  }
}
