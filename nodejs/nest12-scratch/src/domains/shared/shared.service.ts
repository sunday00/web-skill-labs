import { Injectable } from '@nestjs/common'

@Injectable()
export class SharedService {
  private no = 0

  async getNo() {
    return this.no++
  }
}
