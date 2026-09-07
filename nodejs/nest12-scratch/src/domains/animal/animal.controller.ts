import { Body, Controller, Get, Post } from '@nestjs/common'
import { AnimalService } from './animal.service.js'
import {
  AnimalCreateCommand,
  animalCreateSchema,
} from './handlers/animal.create.c.js'
import { SharedService } from '../shared/shared.service.js'

@Controller('animal')
export class AnimalController {
  constructor(
    private readonly animalService: AnimalService,
    private readonly ss: SharedService,
  ) {}

  @Post()
  async create(
    @Body({ schema: animalCreateSchema }) data: AnimalCreateCommand,
  ): Promise<any> {
    return await this.animalService.create(data)
  }

  @Get('/shared')
  public async shared() {
    return this.ss.getNo()
  }
}
