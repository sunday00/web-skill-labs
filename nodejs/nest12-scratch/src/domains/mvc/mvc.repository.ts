import { Injectable } from '@nestjs/common'

@Injectable()
export class MvcRepository {
  private readonly DB = [
    {
      id: '1',
      name: 'Orange',
    },
    {
      id: '2',
      name: 'Blue',
    },
  ] as const

  async findOneHero(id: string) {
    const d = this.DB.find((d) => d.id === id)

    if (!d) throw new Error('NotFound')

    return d
  }
}
