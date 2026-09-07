import { Module } from '@nestjs/common'
import { NestedController } from './nested.controller.js'
import { NestedService } from './nested.service.js'
import { EtcUtil, EtcUtil2 } from '../../utils/etc.utils.js'

@Module({
  controllers: [NestedController],
  providers: [
    NestedService,
    EtcUtil,
    {
      provide: 'EtcUtil2',
      useClass: EtcUtil2,
    },
  ],
})
export class NestedModule {}
