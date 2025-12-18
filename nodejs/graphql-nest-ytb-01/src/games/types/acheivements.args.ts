import { ArgsType, Field, Int } from '@nestjs/graphql'
import { Difficulty } from '../../achievements/types/achievement.type'

@ArgsType()
export class AchievementsArgs {
  @Field(() => Int)
  offset: number

  @Field(() => Int)
  limit: number

  @Field(() => Difficulty, { nullable: true })
  difficulty: Difficulty
}
