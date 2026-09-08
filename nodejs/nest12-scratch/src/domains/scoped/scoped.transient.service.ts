import { Injectable, Scope } from '@nestjs/common'

@Injectable({ scope: Scope.TRANSIENT })
export class ScopedTransientService {
  instanceNo: number

  constructor() {
    this.instanceNo = Math.floor(Math.random() * 100)
  }

  showNo() {
    return 'ts: ' + this.instanceNo
  }
}
