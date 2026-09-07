import { Controller, Get } from '@nestjs/common'
import { NestedService } from './nested.service.js'

@Controller('nested')
export class NestedController {
  constructor(private readonly service: NestedService) {}

  @Get('/greet')
  public async greet() {
    return await this.service.greet()
  }
}
