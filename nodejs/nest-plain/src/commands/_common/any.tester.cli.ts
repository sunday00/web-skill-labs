import { Command, CommandRunner, Option } from 'nest-commander'

@Command({ name: 'test.any', aliases: ['any.test'], description: 'anything for check function' })
export class AnyTesterCli extends CommandRunner {
  constructor() {
    super()
  }

  async run(_passedParams: string[], _options?: any): Promise<void> {}

  @Option({ flags: '-s, --size [number]', description: 'size of rows' })
  size(v: string) {
    return Number(v)
  }
}
