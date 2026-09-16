import { Injectable } from '@nestjs/common'
import { Redis } from 'ioredis'

@Injectable()
export class RedisService {
  public readonly client: Redis
  public readonly sub: Redis

  constructor() {
    this.client = new Redis({
      host: 'localhost',
      port: 6380,
      password: 'kOmedy',
      db: 5,
    })

    this.sub = new Redis({
      host: 'localhost',
      port: 6380,
      password: 'kOmedy',
      db: 6,
    })
  }
}
