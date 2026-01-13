import { Module } from '@nestjs/common'
import { InfraModule } from '@/infra.module'
import { AnyTester } from '@/_commands/_common/any.tester'
import { FruitCmdModule } from '@/_commands/fruit/fruit.cmd.module'

@Module({
  imports: [InfraModule.register(), FruitCmdModule],
  providers: [AnyTester],
})
export class RootCmdModule {}
