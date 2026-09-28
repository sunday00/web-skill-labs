import { Injectable } from '@nestjs/common'

@Injectable()
export class SocketService {
  async handler(data: any) {
    console.log(data)

    return '1'
  }
}
