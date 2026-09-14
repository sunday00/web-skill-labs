import { CommandHandler, ICommand, ICommandHandler } from '@nestjs/cqrs'
import { Inject } from '@nestjs/common'
import { Cache, CACHE_MANAGER } from '@nestjs/cache-manager'

export class AnimalCacheBurstC implements ICommand {}

@CommandHandler(AnimalCacheBurstC)
export class AnimalCacheBurstHandler implements ICommandHandler<AnimalCacheBurstC> {
  constructor(@Inject(CACHE_MANAGER) private cacheManager: Cache) {}

  async execute(query: AnimalCacheBurstC): Promise<any> {
    return await this.cacheManager.clear()
  }
}
