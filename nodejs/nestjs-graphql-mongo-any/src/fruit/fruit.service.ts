import { Injectable } from '@nestjs/common'
import { CreateFruit } from './struct/fruit.dto'
import { InjectModel } from '@nestjs/mongoose'
import { Model } from 'mongoose'
import { Fruit } from './struct/fruit.entity'

@Injectable()
export class FruitService {
  constructor(@InjectModel(Fruit.name) private model: Model<Fruit>) {}

  public async storeFruit(input: CreateFruit) {
    const r = await this.model.create({
      name: input.name,
      lastOrder: input.lastOrder,
    })

    return r
  }

  async storeBulk(dataBag: Omit<Fruit, 'id'>[]) {
    return await this.model.insertMany(dataBag)
  }
}
