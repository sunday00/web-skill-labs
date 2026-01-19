import { Module } from '@nestjs/common'
import { InfraModule } from './infra.module'
import { HelloModule } from './hello/hello.module'
import { UFireBaseModule } from './u-fire-base/u-fire-base.module'
import { FruitModule } from './fruit/fruit.module'
import { DuckdbModule } from './duckdb/duckdb.module';

@Module({
  imports: [
    InfraModule.register({ graphql: true }),
    HelloModule,
    UFireBaseModule,
    FruitModule,
    DuckdbModule,
  ],
  controllers: [],
  providers: [],
})
export class AppModule {}
