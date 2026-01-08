import { NestFactory } from '@nestjs/core'
import { AppModule } from './app.module'
import { ConfigService } from '@nestjs/config'
import { ConsoleLogger } from '@nestjs/common'

async function bootstrap(): Promise<[ConfigService, ConsoleLogger]> {
  const logger = new ConsoleLogger('bootstrap 🚀')
  const app = await NestFactory.create(AppModule, {
    logger,
  })

  app.enableCors({
    allowedHeaders: '*',
    credentials: true,
    origin: ['http://localhost:4000'],
  })

  const config = app.get(ConfigService)

  await app.listen(config.get<number>('env.port', 3000), '0.0.0.0')

  return [config, logger]
}

bootstrap()
  .then(([config, logger]: [ConfigService, ConsoleLogger]) => {
    logger.log(config.get<number>('env.port', 3000))
  })
  .catch((e) => console.error(e))
