import { Command, CommandRunner, Option } from 'nest-commander'
import { SeederOptions } from '@/_commands/_common/seeder.options'
import { FruitService } from '@/fruit/fruit.service'
import { Fruit } from '@/fruit/struct/fruit.entity'
import { randFood, randPastDate } from '@ngneat/falso'

@Command({ name: 'fruit.seed', description: 'seeding fruit' })
export class FruitCmdSeed extends CommandRunner {
  constructor(private readonly service: FruitService) {
    super()
  }

  async run(passedParams: string[], options?: SeederOptions): Promise<void> {
    const size = options?.size ?? 100

    const dataBag: Omit<Fruit, 'id'>[] = []
    for (let i = 0; i < size; i++) {
      dataBag.push({
        name: randFood(),
        lastOrder: randPastDate(),
      })
    }

    await this.service.storeBulk(dataBag)
  }

  @Option({ flags: '-s, --size [number]', description: 'size of rows' })
  size(v: string) {
    return Number(v)
  }
}
