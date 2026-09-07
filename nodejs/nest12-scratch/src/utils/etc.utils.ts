import { Injectable } from '@nestjs/common'

@Injectable()
export class EtcUtil {
  public async greet() {
    return 'hello Guy'
  }
}

@Injectable()
export class EtcUtil2 {
  public async greet() {
    return 'hello Guy'
  }
}
