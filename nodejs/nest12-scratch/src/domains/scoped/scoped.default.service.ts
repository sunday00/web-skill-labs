import { Injectable, Scope } from '@nestjs/common'

@Injectable({ scope: Scope.DEFAULT })
export class ScopedDefaultService {
  instanceNo: number

  constructor() {
    this.instanceNo = Math.floor(Math.random() * 100)
  }

  showNo() {
    return 'ds: ' + this.instanceNo
  }
}
