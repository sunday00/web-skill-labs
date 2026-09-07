import { Controller, Get } from '@nestjs/common'
import { SharedService } from './shared.service.js'

@Controller('shared')
export class SharedController {
  constructor(private readonly service: SharedService) {}

  @Get()
  public async goodman() {
    return 'good'
  }
}
