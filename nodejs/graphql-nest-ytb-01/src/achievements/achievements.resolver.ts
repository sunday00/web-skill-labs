import { Args, Mutation, Resolver } from '@nestjs/graphql'
import { AchievementsService } from './achievements.service'
import { Achievement } from './types/achievement.type'
import { CreateAchievementInput } from './types/achievement.input.create'

@Resolver()
export class AchievementsResolver {
  constructor(private readonly achievementsService: AchievementsService) {}

  @Mutation(() => Achievement, { name: 'createAchievement' })
  public async createAchievement(@Args('input') input: CreateAchievementInput) {
    return await this.achievementsService.createAchievement(input)
  }
}
