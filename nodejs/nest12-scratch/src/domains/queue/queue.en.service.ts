import { Injectable } from '@nestjs/common'
import { InjectFlowProducer, InjectQueue } from '@nestjs/bullmq'
import { FlowProducer, Queue } from 'bullmq'
import { RedisService } from '../../modules/redis/redis.service.js'

@Injectable()
export class QueueEnService {
  constructor(
    @InjectFlowProducer('flows')
    private flows: FlowProducer,

    @InjectQueue('Default')
    private defaultQ: Queue,

    private redis: RedisService,
  ) {}

  async clearQueue() {
    return await Promise.all([
      this.redis.client.flushdb(),
      this.redis.sub.flushdb(),
    ])
  }

  public async createBigJob() {
    return this.flows.add({
      name: 'mother flow',
      queueName: 'Default',
      data: { id: 1, payload: { a: 'apple', b: 'banana' } },
      children: [
        {
          name: 'child job 1',
          queueName: 'Normal2',
          data: { id: '1-1', payload: { c: 'dd', e: 'ff', cnt: 10 } },
        },
        {
          name: 'child job 2',
          queueName: 'Normal2',
          data: { id: '1-2', payload: { c: 'gg', e: 'hh', cnt: 1000 } },
        },
        {
          name: 'child job 3',
          queueName: 'Normal2',
          data: { id: '1-3', payload: { c: 'ii', e: 'jj', cnt: 30 } },
        },
      ],
    })
  }

  async delay(sec: number) {
    return this.defaultQ.add('hello', { foo: 'bar' }, { delay: sec * 1000 })
  }
}
