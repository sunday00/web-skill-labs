import { Controller, Get } from '@nestjs/common'
import { NestedService } from './nested.service.js'
import { SharedService } from '../shared/shared.service.js'

@Controller('nested')
export class NestedController {
  constructor(
    private readonly service: NestedService,
    private readonly ss: SharedService,
  ) {}

  @Get('/greet')
  public async greet() {
    return await this.service.greet()
  }

  @Get('/shared')
  public async shared() {
    return this.ss.getNo()
  }
}
