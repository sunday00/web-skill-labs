import {
  CommandHandler,
  EventBus,
  ICommand,
  ICommandHandler,
} from '@nestjs/cqrs'
import { SagaStep1Done, SagaStep2Done } from './saga.trigger.js'

export class SagaStep2 implements ICommand {
  constructor(public name: string) {}
}

@CommandHandler(SagaStep2)
export class SagaStep2Handler implements ICommandHandler<SagaStep2> {
  constructor(private readonly eb: EventBus) {}

  async execute(command: SagaStep2): Promise<any> {
    console.log('step2: ', command.name)

    this.eb.publish(new SagaStep2Done(command.name))
  }
}
