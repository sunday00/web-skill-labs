import { Module } from '@nestjs/common'
import { OnDemandsService } from './on-demands.service.js'

@Module({
  providers: [OnDemandsService],
  exports: [OnDemandsService],
})
export class OnDemandsModule {}
