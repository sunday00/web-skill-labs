import { IQuery, IQueryHandler, QueryHandler } from '@nestjs/cqrs'
import { faker } from '@faker-js/faker'
import { Inject } from '@nestjs/common'
import { Cache, CACHE_MANAGER } from '@nestjs/cache-manager'

export class AnimalSampleContentQ implements IQuery {}

@QueryHandler(AnimalSampleContentQ)
export class AnimalSampleContentHandler implements IQueryHandler<AnimalSampleContentQ> {
  constructor(@Inject(CACHE_MANAGER) private cacheManager: Cache) {}

  async execute(query: AnimalSampleContentQ): Promise<any> {
    let cached = await this.cacheManager.get('iii')

    if (!cached) {
      cached = faker.number.int()
      await this.cacheManager.set('iii', cached, 10_000)
    }

    return cached
  }
}
