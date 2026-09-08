import { Injectable } from '@nestjs/common'
import { ScopedTransientService } from './scoped.transient.service.js'
import { ScopeConsumerInterface } from './struct/scope.consumer.interface.js'

@Injectable()
export class ScopedTransient1Consumer implements ScopeConsumerInterface {
  public k = 'k1'

  constructor(public readonly service: ScopedTransientService) {}

  showNo() {
    return this.service.showNo()
  }
}
