import { Injectable, Scope } from '@nestjs/common'

@Injectable({ scope: Scope.REQUEST })
export class ScopedRequestService {
  instanceNo: number

  constructor() {
    this.instanceNo = Math.floor(Math.random() * 100)
  }

  showNo() {
    return 'rs: ' + this.instanceNo
  }
}
