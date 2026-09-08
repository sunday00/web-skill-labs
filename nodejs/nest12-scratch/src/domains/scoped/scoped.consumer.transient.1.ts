import { Injectable } from '@nestjs/common'
import { ScopedTransientService } from './scoped.transient.service.js'

@Injectable()
export class ScopedTransient1Consumer {
  constructor(private readonly service: ScopedTransientService) {}

  showNo() {
    return this.service.showNo()
  }
}
