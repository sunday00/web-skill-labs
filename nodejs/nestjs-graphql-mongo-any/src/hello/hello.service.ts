import { Injectable } from '@nestjs/common'
import { JwtService } from '@nestjs/jwt'
import { Model } from 'mongoose'
import { InjectModel } from '@nestjs/mongoose'
import { Hello } from './struct/hello.entity'
import { time } from '../_common/utils/time.util'

@Injectable()
export class HelloService {
  constructor(
    private readonly jwt: JwtService,
    @InjectModel(Hello.name) private model: Model<Hello>,
  ) {}

  public async sayHello() {
    return Promise.resolve('hello')
  }

  public getFakeAccessToken() {
    return this.jwt.sign({ id: -1, nickname: 'tester' })
  }

  public async storeSomething() {
    const r = await this.model.create({
      value: 'test' + time().format('YYYY-MM-DD HH:mm:sss'),
    })

    return r.value
  }
}
