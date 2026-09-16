import { Process, Processor } from '@nestjs/bull'
import { type Job } from 'bull'

@Processor('Default2')
export class QueueBullDefaultConsumer {
  @Process()
  async process(job: Job, token?: string): Promise<any> {}
}

// @Processor('Normal2')
// export class QueueBullNormal2Consumer {
//   @Process()
//   async process(job: Job, token?: string): Promise<any> {}
// }
