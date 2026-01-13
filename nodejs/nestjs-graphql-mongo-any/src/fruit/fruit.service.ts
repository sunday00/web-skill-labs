import { Injectable } from '@nestjs/common'
import { CreateFruit } from './struct/fruit.dto'
import { InjectModel } from '@nestjs/mongoose'
import { Model, MongooseBulkWriteResult } from 'mongoose'
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

  public async storeBulk(dataBag: Omit<Fruit, 'id'>[]) {
    return await this.model.insertMany(dataBag)
  }

  public async findMany(page: number, size: number): Promise<Fruit[]> {
    return await this.model
      .find()
      .skip((page - 1) * size)
      .limit(size)
      .exec()
  }

  public async updateMany(m: Fruit[]): Promise<MongooseBulkWriteResult> {
    return await this.model.bulkWrite(
      m.map((item) => {
        return {
          updateOne: {
            filter: { _id: item.id },
            update: { $set: { producer: item.producer } },
          },
        }
      }),
    )
  }
}
