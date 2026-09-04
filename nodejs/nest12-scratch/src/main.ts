import { NestFactory } from '@nestjs/core'
import { AppModule } from './app.module.js'
import { NestExpressApplication } from '@nestjs/platform-express'
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger'
import { apiReference } from '@scalar/nestjs-api-reference'
import { ValidationPipe } from './aop/pipes/validator.global.js'

async function bootstrap() {
  const app = await NestFactory.create<NestExpressApplication>(AppModule)
  app.set('query parser', 'extended')

  const document = SwaggerModule.createDocument(
    app,
    new DocumentBuilder()
      .setTitle('Practice')
      .setDescription('nestjs some 12')
      .setVersion('12.0')
      .build(),
    {
      autoTagControllers: true,
    },
  )
  app.use('/docs', apiReference({ content: document, theme: 'moon' }))
  SwaggerModule.setup('docs-classic', app, document)

  app.useGlobalPipes(
    new ValidationPipe({
      transform: true,
    }),
  )

  await app.listen(process.env.PORT ?? 8090)

  console.log(process.env.PORT ?? 8090)
}

await bootstrap()
