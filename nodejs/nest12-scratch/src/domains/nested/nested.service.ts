import { Inject, Injectable } from '@nestjs/common'
import { QueryBus } from '@nestjs/cqrs'
import { MotherService } from './mother.service.js'
import { EtcUtil, EtcUtil2 } from '../../utils/etc.utils.js'

@Injectable()
export class NestedService extends MotherService {
  // constructor(private readonly cb: CommandBus, private util: EtcUtil) {
  //   super()
  // }

  @Inject()
  private qb: QueryBus

  @Inject(EtcUtil)
  private readonly util: EtcUtil

  @Inject('EtcUtil2')
  private readonly util2: EtcUtil2

  async greet() {
    return await this.util.greet()
  }
}
