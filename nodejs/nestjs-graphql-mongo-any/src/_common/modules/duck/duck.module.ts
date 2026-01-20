import { DynamicModule, Global, Module } from '@nestjs/common'
import { DuckService } from '@/_common/modules/duck/duck.service'

@Global()
@Module({})
export class DuckModule {
  public static async registerAsync(): Promise<DynamicModule> {
    const provider = new DuckService()
    await provider.init()

    return {
      module: DuckModule,
      providers: [{ provide: DuckService, useValue: provider }],
      exports: [DuckService],
    }
  }
}
