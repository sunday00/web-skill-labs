import {
  ConnectedSocket,
  MessageBody,
  SubscribeMessage,
  WebSocketGateway,
} from '@nestjs/websockets'
import { Socket } from 'socket.io'
import { SocketService } from './socket.service.js'
import { Logger } from '@nestjs/common'

@WebSocketGateway(8091, { namespace: 'events', transports: ['websocket'] })
export class SocketGateway {
  private logger = new Logger(this.constructor.name)

  constructor(private readonly service: SocketService) {}

  @SubscribeMessage('hello')
  async handleEvent(
    // client: Socket,
    // data: string,
    @ConnectedSocket() client: Socket,
    // @MessageBody() data: string,
    @MessageBody('v') v: any,
  ): Promise<any> {
    // this.logger.log(client.client)
    return await this.service.handler(v)
  }
}
