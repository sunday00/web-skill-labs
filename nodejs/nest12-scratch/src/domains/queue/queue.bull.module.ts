import { Module } from '@nestjs/common'
import { QueueBullEnService } from './queue.bull.en.service.js'
import { QueueBullDefaultConsumer } from './queue.bull.consume.service.js'
import { QueueBullController } from './queue.bull.controller.js'
import { BullModule } from '@nestjs/bull'

export const QUEUE_PREFIX = 'nest12-scratch'

@Module({
  imports: [
    BullModule.forRoot('nativeBull', {
      redis: {
        host: 'localhost',
        port: 6380,
        password: 'kOmedy',
        db: 5,
      },
    }),
    BullModule.registerQueue({
      name: 'Default2',
      configKey: 'nativeBull',
    }),
  ],
  controllers: [QueueBullController],
  providers: [
    QueueBullEnService,
    QueueBullDefaultConsumer,
    // QueueBullNormal2Consumer,
    // QueueNormal3Consumer,
  ],
})
export class QueueBullModule {}

/**

 */
