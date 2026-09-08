import { Injectable } from '@nestjs/common'
import { ScopedTransientService } from './scoped.transient.service.js'

@Injectable()
export class ScopedTransient3Consumer {
  constructor(private readonly service: ScopedTransientService) {}

  showNo() {
    return this.service.showNo()
  }
}
