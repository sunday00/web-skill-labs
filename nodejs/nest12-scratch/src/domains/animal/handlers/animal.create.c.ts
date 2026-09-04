import { CommandHandler, ICommand, ICommandHandler } from '@nestjs/cqrs'
import { z } from 'zod'
import { createZodDto } from 'nestjs-zod'

export const animalCreateSchema = z.object({
  name: z.string().default('something'),
  age: z.number().int().positive('not negative'),
})

export class AnimalCreateCommand
  extends createZodDto(animalCreateSchema)
  implements ICommand {}

@CommandHandler(AnimalCreateCommand)
export class AnimalCreateCommandHandler implements ICommandHandler<AnimalCreateCommand> {
  async execute(command: AnimalCreateCommand): Promise<any> {
    console.log(command.name, command.age)

    return 1
  }
}
