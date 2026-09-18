import { Module } from '@nestjs/common'
import { FeatService } from './feat.service.js'
import { FeatController } from './feat.controller.js'

@Module({
  imports: [],
  controllers: [FeatController],
  providers: [FeatService],
})
export class FeatModule {}
