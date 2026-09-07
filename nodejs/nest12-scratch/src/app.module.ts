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

@Module({
  imports: [CqrsModule.forRoot(), AnimalModule, NestedModule],
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
