import { Controller, Get, Param, Render, Res } from '@nestjs/common'
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
}
