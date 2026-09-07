import { Module } from '@nestjs/common'
import { AppController } from './app.controller.js'
import { AppService } from './app.service.js'
import { AnimalModule } from './domains/animal/animal.module.js'
import { CqrsModule } from '@nestjs/cqrs'
import { NestedModule } from './domains/nested/nested.module.js'

@Module({
  imports: [CqrsModule.forRoot(), AnimalModule, NestedModule],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
