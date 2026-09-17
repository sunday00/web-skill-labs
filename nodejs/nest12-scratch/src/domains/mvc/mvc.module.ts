import { Module } from '@nestjs/common'
import { MvcController } from './mvc.controller.js'
import { MvcService } from './mvc.service.js'

@Module({
  controllers: [MvcController],
  providers: [MvcService],
})
export class MVCModule {}
