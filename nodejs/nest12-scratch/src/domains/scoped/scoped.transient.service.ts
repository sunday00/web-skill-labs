import { Inject, Injectable, Scope } from '@nestjs/common'
import { INQUIRER } from '@nestjs/core'
import { type ScopeConsumerInterface } from './struct/scope.consumer.interface.js'

@Injectable({ scope: Scope.TRANSIENT })
export class ScopedTransientService {
  instanceNo: number

  constructor(@Inject(INQUIRER) private readonly inq: ScopeConsumerInterface) {
    this.instanceNo = Math.floor(Math.random() * 100)
  }

  showNo() {
    console.log(this.inq, typeof this.inq, this.inq.k, Object.keys(this.inq))
    return 'ts: ' + this.instanceNo
  }
}
