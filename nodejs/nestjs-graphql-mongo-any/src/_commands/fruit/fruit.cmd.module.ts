import { Module } from '@nestjs/common'
import { FruitCmdSeed } from '@/_commands/fruit/fruit.cmd.seed'
import { FruitModule } from '@/fruit/fruit.module'

@Module({
  imports: [FruitModule],
  providers: [FruitCmdSeed],
})
export class FruitCmdModule {}
