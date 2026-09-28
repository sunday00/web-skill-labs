import { Controller, Get, Post, Render } from '@nestjs/common'
import { SocketGateway } from './socket.gateway.js'

@Controller('socket')
export class SocketController {
  constructor(private readonly gt: SocketGateway) {}

  @Get('/')
  @Render('socket/index')
  public async index() {}

  @Get('/sender')
  @Render('socket/index')
  public async sendFromClient() {
    return { page: 'sender' }
  }

  @Get('/receiver')
  @Render('socket/index')
  public async sendFromServer() {
    return { page: 'receiver' }
  }

  @Post('/emit')
  public async sendEmit() {
    return await this.gt.emit()
  }
}
