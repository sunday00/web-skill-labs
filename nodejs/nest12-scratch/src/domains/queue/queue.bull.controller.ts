import { Controller } from '@nestjs/common'
import { QueueBullEnService } from './queue.bull.en.service.js'

@Controller('queue/bull')
export class QueueBullController {
  constructor(private readonly service: QueueBullEnService) {}
}
