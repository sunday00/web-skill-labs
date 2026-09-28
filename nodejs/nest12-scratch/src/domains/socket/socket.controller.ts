import { Controller, Get, Render } from '@nestjs/common'

@Controller('socket')
export class SocketController {
  @Get('/')
  @Render('socket/index')
  public async index() {}

  @Get('/sender')
  @Render('socket/index')
  public async sendFromClient() {
    return { page: 'sender' }
  }
}
