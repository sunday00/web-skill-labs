import { Module } from '@nestjs/common'
import { InfraModule } from './infra.module'
import { HelloModule } from './hello/hello.module'
import { UFireBaseModule } from './u-fire-base/u-fire-base.module';

@Module({
  imports: [InfraModule, HelloModule, UFireBaseModule],
  controllers: [],
  providers: [],
})
export class AppModule {}
