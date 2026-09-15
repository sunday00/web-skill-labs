import { Processor, WorkerHost } from '@nestjs/bullmq'
import { Job } from 'bullmq'

@Processor('Default')
export class QueueDefaultConsumer extends WorkerHost {
  async process(job: Job, token?: string): Promise<any> {
    console.log(job.data, token, job.name)

    return 1
  }
}

@Processor('Normal2', { concurrency: 5 })
export class QueueNormal2Consumer extends WorkerHost {
  async process(job: Job, token?: string): Promise<any> {
    for (let i = 0; i < job.data.payload.cnt; i++) {
      await new Promise((resolve) => {
        const t = setTimeout(() => {
          console.log(job.name)
          clearTimeout(t)

          resolve(undefined)
        }, 1)
      })
    }

    console.log(job.data, token, job.name)

    return 1 //
  }
}
