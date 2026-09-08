import { Controller, Get } from '@nestjs/common'
import { ApiOperation, ApiTags } from '@nestjs/swagger'
import { ScopedDefault1Consumer } from './scoped.consumer.default.1.js'
import { ScopedDefault3Consumer } from './scoped.consumer.default.3.js'
import { ScopedDefault2Consumer } from './scoped.consumer.default.2.js'

@ApiTags('Scoped')
@Controller('scoped')
export class ScopedSdController {
  constructor(
    private readonly consumer1: ScopedDefault1Consumer,
    private readonly consumer2: ScopedDefault2Consumer,
    private readonly consumer3: ScopedDefault3Consumer,
  ) {}

  @Get('/default')
  @ApiOperation({ description: 'all consumers show same, always same.' })
  showNo() {
    return [
      this.consumer1.showNo(),
      this.consumer2.showNo(),
      this.consumer3.showNo(),
    ]
  }
}
