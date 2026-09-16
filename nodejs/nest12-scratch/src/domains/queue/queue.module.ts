import { Module } from '@nestjs/common'
import { QueueController } from './queue.controller.js'
import { QueueEnService } from './queue.en.service.js'
import {
  QueueDefaultConsumer,
  QueueNormal2Consumer,
} from './queue.consume.service.js'
import { BullModule } from '@nestjs/bullmq'

export const QUEUE_PREFIX = 'nest12-scratch'

@Module({
  imports: [
    BullModule.forRoot('defaultBull', {
      connection: {
        host: 'localhost',
        port: 6380,
        password: 'kOmedy',
        db: 5,
      },
      prefix: QUEUE_PREFIX,
    }),
    BullModule.forRoot('subBull', {
      connection: {
        host: 'localhost',
        port: 6380,
        password: 'kOmedy',
        db: 6,
      },
      prefix: QUEUE_PREFIX,
    }),
    BullModule.registerQueue({
      name: 'Default',
      configKey: 'defaultBull',
      // connection: {
      //   host: 'localhost',
      //   port: 6380,
      //   password: 'kOmedy',
      //   db: 5,
      // },
      // prefix: QUEUE_PREFIX,
    }),
    BullModule.registerQueue({
      name: 'Normal2',
      configKey: 'subBull',
      // connection: {
      //   host: 'localhost',
      //   port: 6380,
      //   password: 'kOmedy',
      //   db: 5,
      // },
      // prefix: QUEUE_PREFIX,
    }),
    BullModule.registerFlowProducer({
      name: 'flows',
      configKey: 'defaultBull',
      // connection: {
      //   host: 'localhost',
      //   port: 6380,
      //   password: 'kOmedy',
      //   db: 5,
      // },
      // prefix: QUEUE_PREFIX,
    }),
  ],
  controllers: [QueueController],
  providers: [QueueEnService, QueueDefaultConsumer, QueueNormal2Consumer],
})
export class QueueModule {}

/**
 * bull q 의 이해
 *
 *  - bull q 는 root를 통해 등록된 큰 stage 가 있고,
 *  - queue 를 통해 하위에 등록한 narrowed 통로가 있다.
 *
 *  - add 를 통해 enqueue 된 job 들은 곧바로 통로로 가지 않고 stage 에 등록된다.
 *  - worker 들은 stage 에서 priority, delay, flow-jobs, lifo 등을 판단해 통로로 가져간다.
 *  - 이제 통로에 차곡차곡 쌓인 job들을 순차적으로 처리한다.
 *
 *  - 이러한 방식이라서 통로에 일단 넣고 순서를 꼬아가며 선행 job을 기다리거나
 *  - 후행 job이 blocking 된다거나
 *  - lifo (last in first out)의 동작이 복잡해지거나 하지 않고,
 *
 *  - stage 에서 delay를 일단 시키고,
 *  - lifo 를 추가하고
 *  - 병행작업들을 동시에 실행하는 복잡한 큐를 단순하게 처리할 수 있게 된다.
 */
