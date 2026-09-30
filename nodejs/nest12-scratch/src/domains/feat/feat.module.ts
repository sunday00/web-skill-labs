import { Module } from '@nestjs/common'
import { FeatService } from './feat.service.js'
import { FeatController } from './feat.controller.js'
import { NestedModule } from '../nested/nested.module.js'
import { ClsModule } from 'nestjs-cls'
import { Request } from 'express'
import { faker } from '@faker-js/faker'

@Module({
  imports: [
    ClsModule.forRoot({
      global: true,
      middleware: {
        mount: true,
        setup: (cls, req: Request) => {
          cls.set(
            'from_nestClsModule',
            new Map<string, any>([['url', req.url]]),
          )
        },
        generateId: true,
        idGenerator: (req: Request) =>
          (req.headers['x-request-id'] as string) ?? faker.string.uuid(),
      },
    }),
  ],
  controllers: [FeatController],
  providers: [FeatService],
})
export class FeatModule implements NestedModule {}
