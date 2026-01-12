import { Injectable, Logger } from '@nestjs/common'
import { JwtService } from '@nestjs/jwt'
import { Model } from 'mongoose'
import { InjectModel } from '@nestjs/mongoose'
import { Hello } from './struct/hello.entity'
import { time } from '../_common/utils/time.util'

@Injectable()
export class HelloService {
  private logger: Logger = new Logger(HelloService.name)

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

  public testLogJson() {
    this.logger.log({ title: 'how to say?', whatIThink: 123 })

    return 'ttt'
  }

  public async storeSomething() {
    const r = await this.model.create({
      value: 'test' + time().format('YYYY-MM-DD HH:mm:sss'),
    })

    return r.value
  }
}
