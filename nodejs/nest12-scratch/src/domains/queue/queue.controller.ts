import { Controller, Delete, Param, Post } from '@nestjs/common'
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

  @Post('/delay/:sec')
  public async delay(@Param('sec') sec: number) {
    return await this.service.delay(sec)
  }

  @Post('/external-woker')
  public async externalWorker() {
    return await this.service.external()
  }
}
