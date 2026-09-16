import { OnWorkerEvent, Processor, WorkerHost } from '@nestjs/bullmq'
import { Job } from 'bullmq'

@Processor('Default')
export class QueueDefaultConsumer extends WorkerHost {
  @OnWorkerEvent('ready')
  async ready(): Promise<any> {
    console.log('ready...')

    return 1
  }

  @OnWorkerEvent('progress')
  async progress(
    job: Job,
    result: string,
    processName: string,
    progress: any,
  ): Promise<any> {
    console.log(
      job.data,
      result,
      job.name,
      processName,
      progress,
      'progress...',
    )

    return 1
  }

  @OnWorkerEvent('stalled')
  async stalled(job: Job, status: string): Promise<any> {
    console.log(job.data, status, job.name, 'stalled...')

    return 1
  } // ?

  @OnWorkerEvent('active')
  async act(job: Job, status: string): Promise<any> {
    console.log(job.data, status, job.name, 'acting...')

    return 1
  }

  async process(job: Job, token?: string): Promise<any> {
    console.log(job.data, token, job.name)

    return 1
  }

  @OnWorkerEvent('completed')
  async complete(
    job: Job,
    result: string,
    processName: string,
    progress: any,
  ): Promise<any> {
    console.log(
      job.data,
      result,
      job.name,
      processName,
      progress,
      'complete...',
    )

    await job.updateProgress(progress)

    return 1
  }

  @OnWorkerEvent('drained')
  async drained(): Promise<any> {
    console.log('drained...')

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
