import { Module } from '@nestjs/common'
import { AnimalController } from './animal.controller.js'
import { AnimalService } from './animal.service.js'
import { AnimalCreateCommandHandler } from './handlers/animal.create.c.js'

@Module({
  controllers: [AnimalController],
  providers: [AnimalService, AnimalCreateCommandHandler],
})
export class AnimalModule {}
