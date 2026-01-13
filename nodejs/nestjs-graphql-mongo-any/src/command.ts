import { CommandFactory } from 'nest-commander'
import { ConsoleLogger } from '@nestjs/common'
import '@/_commands/_common/log.process'
import { RootCmdModule } from './_commands/root.cmd.module'

async function bootstrap() {
  console.log('command on boot....')

  await CommandFactory.run(RootCmdModule, new ConsoleLogger())
}

bootstrap()
  .then(() => {
    console.log('Done')
    process.exit()
  })
  .catch((err) => {
    console.error(err)
  })
