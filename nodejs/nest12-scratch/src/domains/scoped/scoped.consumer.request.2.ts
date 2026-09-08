import { Injectable } from '@nestjs/common'
import { ScopedRequestService } from './scoped.request.service.js'

@Injectable()
export class ScopedRequest2Consumer {
  constructor(private readonly service: ScopedRequestService) {}

  showNo() {
    return this.service.showNo()
  }
}
