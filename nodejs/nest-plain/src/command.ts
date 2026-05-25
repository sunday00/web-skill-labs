import '@/_common/utils/log.prototype'
import '@/_common/utils/mongo.prototype'

import { CommandFactory } from 'nest-commander'
import { ConsoleLogger } from '@nestjs/common'
import { RootCliModule } from '@/commands/root.cli.module'

async function bootstrap() {
  console.log('command on boot....')

  await CommandFactory.run(RootCliModule, new ConsoleLogger())
}

bootstrap()
  .then(() => {
    console.log('Done')
    process.exit()
  })
  .catch((err) => {
    console.error(err)
  })
