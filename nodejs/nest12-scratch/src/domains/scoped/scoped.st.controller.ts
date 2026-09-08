import { Controller, Get } from '@nestjs/common'
import { ApiOperation, ApiTags } from '@nestjs/swagger'
import { ScopedTransient1Consumer } from './scoped.consumer.transient.1.js'
import { ScopedTransient3Consumer } from './scoped.consumer.transient.3.js'
import { ScopedTransient2Consumer } from './scoped.consumer.transient.2.js'

@ApiTags('Scoped')
@Controller('scoped')
export class ScopedTrController {
  constructor(
    private readonly consumer1: ScopedTransient1Consumer,
    private readonly consumer2: ScopedTransient2Consumer,
    private readonly consumer3: ScopedTransient3Consumer,
  ) {}

  @Get('/transient')
  @ApiOperation({
    description: 'different by consumers, but show constantly each.',
  })
  showNo() {
    return [
      this.consumer1.showNo(),
      this.consumer2.showNo(),
      this.consumer3.showNo(),
    ]
  }
}
