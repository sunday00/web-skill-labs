import { Injectable } from '@nestjs/common'
import { WebSocketServer } from '@nestjs/websockets'
import { Server } from 'socket.io'

@Injectable()
export class SocketService {
  @WebSocketServer()
  server: Server

  async handler(data: any) {
    console.log(data)

    return '1'
  }

  async handleToAdmin(v: any) {
    console.log(`to admin: ${v}`)

    return { first: 'you can wait .....' }
  }

  async handleToAdmin2(v: any) {
    console.log(`to admin2: ${v} too`)

    return { second: 'you can wait ........??' }
  }
}
