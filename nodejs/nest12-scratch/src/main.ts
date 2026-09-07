import { NestFactory } from '@nestjs/core'
import { AppModule } from './app.module.js'
import { NestExpressApplication } from '@nestjs/platform-express'
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger'
import { apiReference } from '@scalar/nestjs-api-reference'
import { ValidationPipe } from './aop/pipes/validator.global.js'
import {
  CanBeErrorLog,
  CheckPerformAll,
} from './aop/middlewares/console.middleware.js'
import { NextFunction } from 'express'

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

  app.use(CheckPerformAll)
  app.use(CanBeErrorLog) // <--- this way registered global throwable middleware ---+

  app.useGlobalPipes(
    new ValidationPipe({
      transform: true,
    }),
  )

  app.use((err: Error, req: Request, res: any, next: NextFunction) => {
    console.log('???')
    if (err) {
      // <------------------------- global middleware throw error catch ------------+
      // also able to catch global catch filter
      console.log(err.name)
    }

    return next()
  })

  await app.listen(process.env.PORT ?? 8090)

  console.log(process.env.PORT ?? 8090)
}

await bootstrap()
