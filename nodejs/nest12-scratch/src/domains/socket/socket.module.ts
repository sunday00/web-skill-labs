import { Module, OnModuleInit } from '@nestjs/common'
import { SocketGateway } from './socket.gateway.js'
import { SocketController } from './socket.controller.js'
import hbs from 'hbs'
import path from 'node:path'
import { SocketService } from './socket.service.js'

@Module({
  controllers: [SocketController],
  providers: [SocketGateway, SocketService],
})
export class SocketModule implements OnModuleInit {
  onModuleInit() {
    hbs.registerPartials(
      path.join(import.meta.dirname, '..', '..', '..', 'views', 'socket'),
    )
  }
}
