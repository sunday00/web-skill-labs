import { Injectable } from '@nestjs/common'
import { ModuleRef } from '@nestjs/core'
import { EtcUtil } from '../../utils/etc.utils.js'

@Injectable()
export class RuntimeDiService {
  constructor(private module: ModuleRef) {}

  index(action: string) {
    const util = this.module.get(action === '1' ? EtcUtil : 'EtcUtil2')
    return util.greet()
  }
}
