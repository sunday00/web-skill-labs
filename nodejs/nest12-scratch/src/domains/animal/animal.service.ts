import { Injectable } from '@nestjs/common'
import { CommandBus } from '@nestjs/cqrs'
import { AnimalCreateCommand } from './handlers/animal.create.c.js'

@Injectable()
export class AnimalService {
  constructor(private readonly cb: CommandBus) {}

  async create(data: AnimalCreateCommand) {
    return await this.cb.execute(data)
  }
}
