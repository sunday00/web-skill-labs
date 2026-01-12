import { Field, InputType } from '@nestjs/graphql'
import { Fruit } from './fruit.entity'
import { IsDate } from 'class-validator'

@InputType()
export class CreateFruit implements Omit<Fruit, 'id'> {
  @Field()
  name: string

  @Field(() => Date)
  @IsDate()
  lastOrder: Date
}
