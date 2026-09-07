import { Module } from '@nestjs/common'
import { NestedController } from './nested.controller.js'
import { NestedService } from './nested.service.js'
import { EtcUtil, EtcUtil2 } from '../../utils/etc.utils.js'
import { SharedModule } from '../shared/shared.module.js'

@Module({
  imports: [SharedModule], // <-- exports and import way makes providers singleton
  controllers: [NestedController],
  providers: [
    NestedService,
    EtcUtil,
    {
      provide: 'EtcUtil2',
      useClass: EtcUtil2,
    },
    // SharedService, // <-- this way is not singleton works
  ],
})
export class NestedModule {}
