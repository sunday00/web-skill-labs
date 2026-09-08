import { Module } from '@nestjs/common'
import { NestedController } from './nested.controller.js'
import { NestedService } from './nested.service.js'
import { EtcUtil, EtcUtil2 } from '../../utils/etc.utils.js'
import { SharedModule } from '../shared/shared.module.js'
import { CircleService } from './circle.service.js'
import { Circle1Handler } from './circle1.handler.js'
import { Circle2Handler } from './circle2.handler.js'
import { RuntimeDiService } from './runtime.di.service.js'

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

    CircleService,
    Circle1Handler,
    Circle2Handler,

    RuntimeDiService,
  ],
})
export class NestedModule {}
