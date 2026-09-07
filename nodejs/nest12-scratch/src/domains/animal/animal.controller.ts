import {
  Body,
  Controller,
  Get,
  HttpException,
  HttpStatus,
  Post,
} from '@nestjs/common'
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

  @Get('/err-mid')
  public async errFromMiddleware() {
    return 1
  }

  @Get('/err-cont')
  public async errFromThis() {
    try {
      return await this.animalService.errFromController()
    } catch (error) {
      throw new HttpException(
        {
          status: HttpStatus.BAD_REQUEST,
          error: '????',

          errorCode: 'FUCKED',
          message: 'this method fucked intended',
        },
        HttpStatus.FORBIDDEN,
        { cause: error }, // for dev tracking inner error detail
      )
    }
  }
}
