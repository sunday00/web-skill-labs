import { Module } from '@nestjs/common'
import { InfraConfigModule } from '@/infra.config.module'
import { AnyTesterCli } from '@/commands/_common/any.tester.cli'

@Module({
  imports: [InfraConfigModule.register()],
  providers: [AnyTesterCli],
})
export class RootCliModule {}
