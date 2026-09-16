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
