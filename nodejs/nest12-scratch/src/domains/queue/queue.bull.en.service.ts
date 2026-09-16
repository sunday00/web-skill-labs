import { Injectable } from '@nestjs/common'
import { RedisService } from '../../modules/redis/redis.service.js'

@Injectable()
export class QueueBullEnService {
  constructor(private redis: RedisService) {}
}
