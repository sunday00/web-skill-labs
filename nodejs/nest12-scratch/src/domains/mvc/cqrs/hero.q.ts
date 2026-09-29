import { IQueryHandler, QueryHandler } from '@nestjs/cqrs'
import { MvcRepository } from '../mvc.repository.js'
import { Hero } from '../model/hero.model.js'
import { plainToInstance } from 'class-transformer'

export class HeroQ {
  constructor(public readonly id: string) {}
}

@QueryHandler(HeroQ)
export class HeroHandler implements IQueryHandler<HeroQ> {
  constructor(private readonly repository: MvcRepository) {}
  async execute(query: HeroQ): Promise<Hero> {
    const d = await this.repository.findOneHero(query.id)

    return plainToInstance(Hero, d)
  }
}
