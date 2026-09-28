import { Injectable } from '@nestjs/common'

@Injectable()
export class SocketService {
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
