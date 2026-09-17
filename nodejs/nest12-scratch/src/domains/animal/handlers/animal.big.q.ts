import { IQuery, IQueryHandler, QueryHandler } from '@nestjs/cqrs'
import { faker } from '@faker-js/faker'
import { Inject } from '@nestjs/common'
import { Cache, CACHE_MANAGER } from '@nestjs/cache-manager'

export class AnimalBigQ implements IQuery {}

@QueryHandler(AnimalBigQ)
export class AnimalBigHandler implements IQueryHandler<AnimalBigQ> {
  constructor(@Inject(CACHE_MANAGER) private cacheManager: Cache) {}

  async execute(query: AnimalBigQ): Promise<any> {
    let cached = await this.cacheManager.get('big')

    if (!cached) {
      cached = Array.from({ length: 500 }, (_, i) => {
        return {
          id: i,
          name: faker.person.firstName(),
          brief: faker.lorem.sentences(50),
        }
      })
      await this.cacheManager.set('big', cached, 20_000)
    }

    return cached
  }
}
