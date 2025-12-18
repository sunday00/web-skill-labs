import { ArgsType, Field, Int } from '@nestjs/graphql'
import { Difficulty } from '../../achievements/types/achievement.type'

@ArgsType()
export class PaginationArgs {
  @Field(() => Int)
  offset: number

  @Field(() => Int)
  limit: number
}

@ArgsType()
export class AchievementsArgs extends PaginationArgs {
  @Field(() => Difficulty, { nullable: true })
  difficulty: Difficulty
}
