import { Module } from '@nestjs/common'
import { AppController } from './app.controller.js'
import { AppService } from './app.service.js'
import { AnimalModule } from './domains/animal/animal.module.js'
import { CqrsModule } from '@nestjs/cqrs'

@Module({
  imports: [CqrsModule.forRoot(), AnimalModule],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
