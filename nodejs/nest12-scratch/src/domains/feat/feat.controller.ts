import { Controller, Get, Param, Post, Render, Req, Sse } from '@nestjs/common'
import { type Request } from 'express'
import { FeatService } from './feat.service.js'

@Controller('feat')
export class FeatController {
  constructor(private readonly service: FeatService) {}

  @Get('sse')
  @Render('feat/sse')
  public async index() {
    return await this.service.index()
  }

  @Sse('sse/subscribe')
  public async sseSubscribeEndPoint() {
    return await this.service.subscribe()
  }

  @Post('send/:msg')
  public async send(@Req() req: Request, @Param('msg') msg: string) {
    return await this.service.send(req, msg)
  }
}
