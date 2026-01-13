import { Command, CommandRunner, Option } from 'nest-commander'
import { SeederOptions } from '@/_commands/_common/seeder.options'
import { FruitService } from '@/fruit/fruit.service'

@Command({ name: 'fruit.seed', description: 'seeding fruit' })
export class FruitCmdSeed extends CommandRunner {
  constructor(private readonly service: FruitService) {
    super()
  }

  async run(passedParams: string[], options?: SeederOptions): Promise<void> {
    const size = options?.size ?? 100

    console.log(size)
  }

  @Option({ flags: '-s, --size [number]', description: 'size of rows' })
  size(v: string) {
    return Number(v)
  }
}
