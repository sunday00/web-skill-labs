import { Module } from '@nestjs/common'
import { FruitCmdSeed } from '@/_commands/fruit/fruit.cmd.seed'
import { FruitModule } from '@/fruit/fruit.module'
import { FruitCmdAddProducer } from '@/_commands/fruit/fruit.cmd.add-producer'

@Module({
  imports: [FruitModule],
  providers: [FruitCmdSeed, FruitCmdAddProducer],
})
export class FruitCmdModule {}
