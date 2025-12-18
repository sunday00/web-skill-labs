import { Field, InputType, Int } from '@nestjs/graphql'
import { Difficulty } from './achievement.type'

@InputType()
export class CreateAchievementInput {
  @Field()
  title: string

  @Field()
  description: string

  @Field(() => Int, { nullable: true })
  points: number

  @Field(() => Difficulty)
  difficulty: Difficulty

  @Field()
  gameId: string
}
