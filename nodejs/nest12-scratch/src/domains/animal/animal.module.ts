import { Module } from '@nestjs/common'
import { AnimalController } from './animal.controller.js'
import { AnimalService } from './animal.service.js'
import { AnimalCreateCommandHandler } from './handlers/animal.create.c.js'
import { SharedModule } from '../shared/shared.module.js'
import { DiscoveryModule } from '@nestjs/core'
import { AnimalSampleContentHandler } from './handlers/animal.sample.content.q.js'
import { CacheModule } from '@nestjs/cache-manager'
import { AnimalCacheBurstHandler } from './handlers/animal.cache.burst.c.js'

@Module({
  imports: [SharedModule, DiscoveryModule, CacheModule.register()],
  controllers: [AnimalController],
  providers: [
    AnimalService,
    AnimalCreateCommandHandler,
    // SharedService
    AnimalSampleContentHandler,
    AnimalCacheBurstHandler,
  ],
})
export class AnimalModule {}
