import { Injectable } from '@nestjs/common'
import { ScopedTransientService } from './scoped.transient.service.js'
import { ScopeConsumerInterface } from './struct/scope.consumer.interface.js'

@Injectable()
export class ScopedTransient3Consumer implements ScopeConsumerInterface {
  public k = 'k3'

  constructor(public readonly service: ScopedTransientService) {}

  showNo() {
    return this.service.showNo()
  }
}
