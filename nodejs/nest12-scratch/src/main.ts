import { NestFactory } from '@nestjs/core'
import { AppModule, ObserveInstrument } from './app.module.js'
import { NestExpressApplication } from '@nestjs/platform-express'
import { DocumentBuilder, SwaggerModule } from '@nestjs/swagger'
import { apiReference } from '@scalar/nestjs-api-reference'
import { ValidationPipe } from './aop/pipes/validator.global.js'
import {
  CanBeErrorLog,
  CheckPerformAll,
} from './aop/middlewares/console.middleware.js'
import { NextFunction, Request, Response } from 'express'
import { ConsoleLogger } from '@nestjs/common'
import compression from 'compression'

async function bootstrap() {
  const app = await NestFactory.create<NestExpressApplication>(AppModule, {
    instrument: ObserveInstrument,
    logger: new ConsoleLogger({
      depth: 10,
      showHidden: true,
    }),
  })
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

  app.use(compression())

  app.use((req: Request, res: Response, next: NextFunction) => {
    console.log('req Accept-Encoding =', req.headers['accept-encoding'])
    res.on('finish', () => {
      console.log('res Content-Encoding =', res.getHeader('content-encoding'))
    })
    next()

    /**
     * comparession 은 육안 구분 swagger ui 등에서는 browser 때문에 잘 안보임
     *
     * curl 로는 잘 보이고,
     * curl -s -H 'Accept-Encoding: identity' http://localhost:8090/animal/big \
     *     -o /dev/null \
     *     -w 'size_download=%{size_download} bytes\n'
     *
     * curl -s -H 'Accept-Encoding: br, gzip' http://localhost:8090/animal/big \
     *     -o /dev/null \
     *     -w 'encoding=%{content_type}\nsize_download=%{size_download} bytes\n'
     *
     * 이렇게 하면 br, gzip 에서 잘 줄어든 거 볼 수 있음.
     *
     **/
  })

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
