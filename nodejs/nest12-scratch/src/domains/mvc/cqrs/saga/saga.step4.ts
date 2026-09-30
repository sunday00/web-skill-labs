import {
  CommandHandler,
  EventBus,
  ICommand,
  ICommandHandler,
} from '@nestjs/cqrs'
import { SagaStep4Done } from './saga.trigger.js'

export class SagaStep4 implements ICommand {
  constructor(public name: string) {}
}

@CommandHandler(SagaStep4)
export class SagaStep4Handler implements ICommandHandler<SagaStep4> {
  constructor(private readonly eb: EventBus) {}

  async execute(command: SagaStep4): Promise<any> {
    console.log('step4: ', command.name)

    this.eb.publish(new SagaStep4Done(command.name))
  }
}
