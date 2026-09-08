import { forwardRef, Inject, Injectable } from '@nestjs/common'
import { Circle2Handler } from './circle2.handler.js'

@Injectable()
export class Circle1Handler {
  constructor(@Inject(forwardRef(() => Circle2Handler)) private sibling: any) {}

  k() {
    return this.sibling.v()
  }

  v() {
    return '1'
  }
}
