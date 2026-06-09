import { Module } from '@nestjs/common'
import { InfraConfigModule } from './infra.config.module'

@Module({
  imports: [InfraConfigModule.register({ addToImports: [] })],
  controllers: [],
  providers: [],
})
export class AppModule {}
