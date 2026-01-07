import { Injectable } from '@nestjs/common'
import { JwtService } from '@nestjs/jwt'

@Injectable()
export class HelloService {
  constructor(private readonly jwt: JwtService) {}

  public async sayHello() {
    return Promise.resolve('hello')
  }

  public getFakeAccessToken() {
    return this.jwt.sign({ id: -1, nickname: 'tester' })
  }
}
