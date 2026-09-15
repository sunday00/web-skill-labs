import { Injectable } from '@nestjs/common'
import { InjectFlowProducer } from '@nestjs/bullmq'
import { FlowProducer } from 'bullmq'
import { RedisService } from '../../modules/redis/redis.service.js'

@Injectable()
export class QueueEnService {
  constructor(
    @InjectFlowProducer('flows')
    private flows: FlowProducer,

    private redis: RedisService,
  ) {}

  async clearQueue() {
    return await this.redis.client.flushdb()
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
}
