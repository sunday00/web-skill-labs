import { Controller, Get, Param, Post, Render, Res } from '@nestjs/common'
import { MvcService } from './mvc.service.js'
import { type Response } from 'express'

@Controller('mvc')
export class MvcController {
  constructor(private readonly service: MvcService) {}

  @Get()
  @Render('index')
  public async index() {
    return await this.service.index()
  }

  @Get('/:view')
  public async dynamicView(@Res() res: Response, @Param('view') view: string) {
    return await this.service.responseWithView(res, view)
  }

  @Get('/cqrs/q')
  public async cqrsQ() {
    return await this.service.cqrsQ()
  }

  @Get('/cqrs/e')
  public async cqrsE() {
    return await this.service.cqrsE()
  }

  @Post('/cqrs/saga')
  public async cqrsSaga() {
    return await this.service.saga()
  }
}
