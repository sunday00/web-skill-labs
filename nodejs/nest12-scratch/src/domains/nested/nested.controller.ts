import { Controller, Get, Param } from '@nestjs/common'
import { NestedService } from './nested.service.js'
import { SharedService } from '../shared/shared.service.js'
import { CircleService } from './circle.service.js'
import { RuntimeDiService } from './runtime.di.service.js'

@Controller('nested')
export class NestedController {
  constructor(
    private readonly service: NestedService,
    private readonly ss: SharedService,
    private readonly cs: CircleService,
    private readonly rd: RuntimeDiService,
  ) {}

  @Get('/greet')
  public async greet() {
    return await this.service.greet()
  }

  @Get('/shared')
  public async shared() {
    return this.ss.getNo()
  }

  @Get('/circle')
  public async circle() {
    return this.cs.circle()
  }

  @Get('/rd/:action')
  public runTimeDi(@Param('action') action: string) {
    return this.rd.index(action)
  }
}
