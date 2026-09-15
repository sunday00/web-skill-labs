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
    // BullModule.forRoot({
    //   connection: {
    //     host: 'localhost',
    //     port: 6380,
    //     password: 'kOmedy',
    //     db: 5,
    //   },
    //   prefix: QUEUE_PREFIX,
    // }),
    BullModule.registerQueue({
      name: 'Default',
      connection: {
        host: 'localhost',
        port: 6380,
        password: 'kOmedy',
        db: 5,
      },
      prefix: QUEUE_PREFIX,
    }),
    BullModule.registerQueue({
      name: 'Normal2',
      connection: {
        host: 'localhost',
        port: 6380,
        password: 'kOmedy',
        db: 5,
      },
      prefix: QUEUE_PREFIX,
    }),
    BullModule.registerFlowProducer({
      name: 'flows',
      connection: {
        host: 'localhost',
        port: 6380,
        password: 'kOmedy',
        db: 5,
      },
      prefix: QUEUE_PREFIX,
    } as any),
  ],
  controllers: [QueueController],
  providers: [QueueEnService, QueueDefaultConsumer, QueueNormal2Consumer],
})
export class QueueModule {}
