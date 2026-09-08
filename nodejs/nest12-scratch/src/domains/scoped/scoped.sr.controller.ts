import { Controller, Get } from '@nestjs/common'
import { ApiOperation, ApiTags } from '@nestjs/swagger'
import { ScopedRequest1Consumer } from './scoped.consumer.request.1.js'
import { ScopedRequest2Consumer } from './scoped.consumer.request.2.js'
import { ScopedRequest3Consumer } from './scoped.consumer.request.3.js'

@ApiTags('Scoped')
@Controller('scoped')
export class ScopedSrController {
  constructor(
    private readonly consumer1: ScopedRequest1Consumer,
    private readonly consumer2: ScopedRequest2Consumer,
    private readonly consumer3: ScopedRequest3Consumer,
  ) {}

  @Get('/request')
  @ApiOperation({
    description: 'all consumers show same, different by request.',
  })
  showNo() {
    return [
      this.consumer1.showNo(),
      this.consumer2.showNo(),
      this.consumer3.showNo(),
    ]
  }
}
