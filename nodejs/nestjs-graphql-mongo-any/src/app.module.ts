import { Module } from '@nestjs/common'
import { InfraModule } from './infra.module'
import { HelloModule } from './hello/hello.module';

@Module({
  imports: [InfraModule, HelloModule],
  controllers: [],
  providers: [],
})
export class AppModule {}
