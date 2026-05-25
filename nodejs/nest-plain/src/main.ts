import { NestFactory } from '@nestjs/core'
import { AppModule } from './app.module'
import { ConsoleLogger, INestApplication, RequestMethod, VersioningType } from '@nestjs/common'
import { ENV } from './_configs/env.config'
import { ConfigService } from '@nestjs/config'
import { applySwagger } from './_configs/swagger.config'

async function bootstrap() {
  const logger = new ConsoleLogger('🐱 CoreApi')
  const app = await NestFactory.create(AppModule, {
    logger,
  })
  const port = Number(ENV.get('APP_PORT', 3000))

  app
    .enableVersioning({
      type: VersioningType.URI,
      defaultVersion: ['1'],
    })
    .setGlobalPrefix('api', {
      exclude: [
        { path: 'graphql', method: RequestMethod.ALL, version: '' },
        { path: 'docs/*path', method: RequestMethod.GET, version: '' },
        { path: '_*path{/*path}', method: RequestMethod.ALL, version: '' },
      ],
    })

  applySwagger(app)

  await app.listen(process.env.PORT ?? port, '0.0.0.0')

  return [app, logger]
}
bootstrap()
  .then(([app, logger]: [INestApplication, ConsoleLogger]) => {
    const config = app.get(ConfigService)

    logger.log(`API is now on ${config.get('APP_PORT')}`)
  })
  .catch((e) => {
    console.error(e)
  })
