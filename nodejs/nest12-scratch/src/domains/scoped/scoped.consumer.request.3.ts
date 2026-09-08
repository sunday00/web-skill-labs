import { Injectable } from '@nestjs/common'
import { ScopedRequestService } from './scoped.request.service.js'

@Injectable()
export class ScopedRequest3Consumer {
  constructor(private readonly service: ScopedRequestService) {}

  showNo() {
    return this.service.showNo()
  }
}
