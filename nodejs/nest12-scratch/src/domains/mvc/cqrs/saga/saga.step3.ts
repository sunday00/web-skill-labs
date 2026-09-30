import {
  CommandHandler,
  EventBus,
  ICommand,
  ICommandHandler,
} from '@nestjs/cqrs'
import { SagaStep3Done } from './saga.trigger.js'

export class SagaStep3 implements ICommand {
  constructor(public name: string) {}
}

@CommandHandler(SagaStep3)
export class SagaStep3Handler implements ICommandHandler<SagaStep3> {
  constructor(private readonly eb: EventBus) {}

  async execute(command: SagaStep3): Promise<any> {
    console.log('step3: ', command.name)

    this.eb.publish(new SagaStep3Done(command.name))
  }
}
