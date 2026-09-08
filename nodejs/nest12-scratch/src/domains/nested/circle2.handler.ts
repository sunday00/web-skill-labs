import { forwardRef, Inject, Injectable } from '@nestjs/common'
import { Circle1Handler } from './circle1.handler.js'

@Injectable()
export class Circle2Handler {
  constructor(@Inject(forwardRef(() => Circle1Handler)) private sibling: any) {}

  k() {
    return this.sibling.v()
  }

  v() {
    return '2'
  }
}
