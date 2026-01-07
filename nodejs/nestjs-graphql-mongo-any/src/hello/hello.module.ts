import { Module } from '@nestjs/common'
import { HelloService } from './hello.service'
import { HelloResolver } from './hello.resolver'
import { MongooseModule } from '@nestjs/mongoose'
import { Hello, HelloSchema } from './struct/hello.entity'

@Module({
  imports: [
    MongooseModule.forFeature([{ name: Hello.name, schema: HelloSchema }]),
  ],
  providers: [HelloResolver, HelloService],
})
export class HelloModule {}
