import { Injectable } from '@nestjs/common'
import { ScopedDefaultService } from './scoped.default.service.js'

@Injectable()
export class ScopedDefault3Consumer {
  constructor(private readonly service: ScopedDefaultService) {}

  showNo() {
    return this.service.showNo()
  }
}
