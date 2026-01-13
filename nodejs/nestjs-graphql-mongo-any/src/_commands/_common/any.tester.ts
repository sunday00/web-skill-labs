import { Command, CommandRunner, Option } from 'nest-commander'
import { SeederOptions } from '@/_commands/_common/seeder.options'

@Command({ name: 'test.any', description: 'anything for check function' })
export class AnyTester extends CommandRunner {
  constructor() {
    super()
  }

  async run(passedParams: string[], options?: SeederOptions): Promise<void> {}

  @Option({ flags: '-s, --size [number]', description: 'size of rows' })
  size(v: string) {
    return Number(v)
  }
}
