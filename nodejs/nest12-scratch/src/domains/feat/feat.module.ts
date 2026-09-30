import { Module } from '@nestjs/common'
import { FeatService } from './feat.service.js'
import { FeatController } from './feat.controller.js'
import { NestedModule } from '../nested/nested.module.js'

@Module({
  imports: [],
  controllers: [FeatController],
  providers: [FeatService],
})
export class FeatModule implements NestedModule {}
