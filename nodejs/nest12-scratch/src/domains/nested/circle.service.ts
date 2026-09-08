import { Injectable } from '@nestjs/common'
import { Circle1Handler } from './circle1.handler.js'
import { Circle2Handler } from './circle2.handler.js'

@Injectable()
export class CircleService {
  constructor(
    private readonly circleHandler1: Circle1Handler,
    private readonly circleHandler2: Circle2Handler,
  ) {}

  circle() {
    return [this.circleHandler1.k(), this.circleHandler2.k()]
  }
}
