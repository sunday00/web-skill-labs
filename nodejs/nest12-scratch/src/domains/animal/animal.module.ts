import { Module } from '@nestjs/common'
import { AnimalController } from './animal.controller.js'
import { AnimalService } from './animal.service.js'
import { AnimalCreateCommandHandler } from './handlers/animal.create.c.js'
import { SharedModule } from '../shared/shared.module.js'
import { DiscoveryModule } from '@nestjs/core'

@Module({
  imports: [SharedModule, DiscoveryModule],
  controllers: [AnimalController],
  providers: [
    AnimalService,
    AnimalCreateCommandHandler,
    // SharedService
  ],
})
export class AnimalModule {}
