import { DocumentBuilder, SwaggerCustomOptions, SwaggerModule } from '@nestjs/swagger'
import { INestApplication } from '@nestjs/common'
import { ENV } from './env.config'

const description = `## test for any plain nestjs`

export const applySwagger = (app: INestApplication) => {
  if (ENV['APP_ENV'] === 'prod' || ENV['APP_ENV'] === 'production') return

  const config = new DocumentBuilder()
    .setTitle('Plain nest test')
    .setDescription(description)
    .setVersion('1.0')
    //
    .addServer(ENV['APP_HOST'] ?? '')
    .addServer(ENV['APP_DEPLOY'] ?? '')
    .addServer(ENV['APP_PROD'] ?? '')
    .addBearerAuth(
      {
        type: 'http',
        in: 'header',
        scheme: 'bearer',
        name: 'jwt',
      },
      'bearer',
    )
    .addApiKey(
      {
        type: 'apiKey',
        in: 'header',
        name: 'api-key',
      },
      'api-key',
    )
    .addSecurityRequirements('bearer')

    .build()

  const customOptions: SwaggerCustomOptions = {
    swaggerOptions: {
      persistAuthorization: true,
    },
  }

  const documentFactory = () => SwaggerModule.createDocument(app, config)
  SwaggerModule.setup('docs', app, documentFactory, customOptions)
}
