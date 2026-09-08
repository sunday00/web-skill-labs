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

export const { ObserveModule, ObserveInstrument } = createObserveModule()

@Module({
  imports: [
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
    CqrsModule.forRoot(),
    AnimalModule,
    NestedModule,
    ScopedModule,
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
