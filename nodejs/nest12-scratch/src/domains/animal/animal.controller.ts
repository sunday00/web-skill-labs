import { Body, Controller, Post } from '@nestjs/common'
import { AnimalService } from './animal.service.js'
import {
  AnimalCreateCommand,
  animalCreateSchema,
} from './handlers/animal.create.c.js'

@Controller('animal')
export class AnimalController {
  constructor(private readonly animalService: AnimalService) {}

  @Post()
  async create(
    @Body({ schema: animalCreateSchema }) data: AnimalCreateCommand,
  ): Promise<any> {
    return await this.animalService.create(data)
  }
}
