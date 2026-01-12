import { Args, Mutation, Resolver } from '@nestjs/graphql'
import { FruitService } from './fruit.service'
import { Fruit } from './struct/fruit.entity'
import { CreateFruit } from './struct/fruit.dto'

@Resolver()
export class FruitResolver {
  constructor(private readonly fruitService: FruitService) {}

  @Mutation(() => Fruit, { name: 'createFruit' })
  public async storeFruit(@Args('input') input: CreateFruit) {
    return this.fruitService.storeFruit(input)
  }
}
