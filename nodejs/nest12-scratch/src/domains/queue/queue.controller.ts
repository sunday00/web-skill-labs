import { Controller, Delete, Post } from '@nestjs/common'
import { QueueEnService } from './queue.en.service.js'

@Controller('queue')
export class QueueController {
  constructor(private readonly service: QueueEnService) {}

  @Delete('/clear')
  public async clear() {
    return await this.service.clearQueue()
  }


  @Post('/flow')
  public async flowJobs() {
    return await this.service.createBigJob()
  }
}
