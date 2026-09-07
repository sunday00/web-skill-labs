import { Module } from '@nestjs/common'
import { SharedController } from './shared.controller.js'
import { SharedService } from './shared.service.js'

@Module({
  controllers: [SharedController],
  providers: [SharedService],
  exports: [SharedService],
})
export class SharedModule {}
