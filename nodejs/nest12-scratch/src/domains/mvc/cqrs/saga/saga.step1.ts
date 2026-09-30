import {
  CommandHandler,
  EventBus,
  ICommand,
  ICommandHandler,
} from '@nestjs/cqrs'
import { SagaStep1Done } from './saga.trigger.js'

export class SagaStep1 implements ICommand {
  constructor(public name: string) {}
}

@CommandHandler(SagaStep1)
export class SagaStep1Handler implements ICommandHandler<SagaStep1> {
  constructor(private readonly eb: EventBus) {}

  async execute(command: SagaStep1): Promise<any> {
    console.log('step1: ', command.name)

    this.eb.publish(new SagaStep1Done(command.name))
  }
}
