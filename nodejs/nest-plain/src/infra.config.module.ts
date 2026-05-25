import { DynamicModule, Logger, Module, ModuleMetadata } from '@nestjs/common'
import { ConfigModule } from '@nestjs/config'
import { envs } from './_configs/env.config'
import { CqrsModule } from '@nestjs/cqrs'

@Module({})
export class InfraConfigModule {
  static register(options?: { addToImports: ModuleMetadata['imports'] }): DynamicModule {
    const _logger: Logger = new Logger(InfraConfigModule.name + '🤖')

    const imports: ModuleMetadata['imports'] = [
      ConfigModule.forRoot({
        isGlobal: true,
        load: [envs],
      }),

      CqrsModule.forRoot(),
    ]

    const providers = []

    if ((options?.addToImports?.length ?? 0) > 0) {
      options!.addToImports!.forEach((addToImport) => {
        imports.push(addToImport)
      })
    }

    const exports = [ConfigModule]

    return {
      module: InfraConfigModule,
      imports,
      providers,
      exports,
    }
  }
}
