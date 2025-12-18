import { Field, Int, ObjectType } from '@nestjs/graphql'
import { Achievement } from '../../achievements/types/achievement.type'

@ObjectType()
export class Game {
  @Field()
  id: string

  @Field()
  name: string

  @Field({ nullable: true, description: 'rpg, shooting...' })
  genre: string

  @Field(() => Int, { nullable: true })
  price: number

  @Field(() => [Achievement], { nullable: true })
  achievements: Achievement[]
}
