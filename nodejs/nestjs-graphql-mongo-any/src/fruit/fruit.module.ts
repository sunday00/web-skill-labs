import { Module } from '@nestjs/common'
import { FruitService } from './fruit.service'
import { FruitResolver } from './fruit.resolver'
import { MongooseModule } from '@nestjs/mongoose'
import { Fruit, FruitSchema } from './struct/fruit.entity'

@Module({
  imports: [
    MongooseModule.forFeature([{ name: Fruit.name, schema: FruitSchema }]),
  ],
  providers: [FruitResolver, FruitService],
  exports: [FruitService],
})
export class FruitModule {}
