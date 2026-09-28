import {
  Ack,
  ConnectedSocket,
  MessageBody,
  SubscribeMessage,
  WebSocketGateway,
  WebSocketServer,
} from '@nestjs/websockets'
import { Server, Socket } from 'socket.io'
import { SocketService } from './socket.service.js'
import { Logger } from '@nestjs/common'

@WebSocketGateway(8091, { namespace: 'events', transports: ['websocket'] })
export class SocketGateway {
  @WebSocketServer()
  server: Server

  private logger = new Logger(this.constructor.name)

  constructor(private readonly service: SocketService) {}

  @SubscribeMessage('hello')
  async handleHello(
    // client: Socket,
    // data: string,
    @ConnectedSocket() client: Socket,
    // @MessageBody() data: string,
    @MessageBody('v') v: any,
  ): Promise<any> {
    // this.logger.log(client.client)
    return await this.service.handler(v)
  }

  @SubscribeMessage('toAdmin')
  async handleToAdmin(
    @ConnectedSocket() client: Socket,
    @MessageBody('v') v: any,
  ): Promise<any> {
    return await this.service.handleToAdmin(v)
  }

  // response first then handle after that
  @SubscribeMessage('toBanana')
  async handleToBanana(
    @MessageBody('v') v: any,
    @Ack() ack: (res: any) => void,
  ): Promise<any> {
    ack({ status: 'OK', data: 'fooo' })

    return this.service.handler(v)
  }

  @SubscribeMessage('toAdmin')
  async handleToAdmin2(@MessageBody('v') v: any): Promise<any> {
    return await this.service.handleToAdmin2(v)
  }

  async emit() {
    return await this.server.emit('serverSent', { message: 'shho' })
  }
}
