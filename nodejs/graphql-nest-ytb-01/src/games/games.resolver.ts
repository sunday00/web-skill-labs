import { Args, Info, Int, Mutation, Parent, Query, ResolveField, Resolver } from '@nestjs/graphql'
import { GamesService } from './games.service'
import { Game } from './types/game.type'
import { Achievement } from '../achievements/types/achievement.type'
import { AchievementsService } from '../achievements/achievements.service'
import { AchievementsArgs } from './types/acheivements.args'
import { CreateGameInput } from './types/game.input.create'
import { GraphQLResolveInfo } from 'graphql/type'

@Resolver(() => Game)
export class GamesResolver {
  constructor(
    private readonly gamesService: GamesService,
    private readonly achievementsService: AchievementsService,
  ) {}

  @Query(() => [Game], { name: 'games' })
  public async getGames(
    @Args('offset', { type: () => Int }) offset: number,
    @Args('limit', { type: () => Int }) limit: number,
  ) {
    return await this.gamesService.getGames(offset, limit)
  }

  @Query(() => Game, { name: 'game' })
  public async getGameById(@Args('id') id: string, @Info() info: GraphQLResolveInfo) {
    console.log(info.fieldNodes[0].selectionSet.selections.map((s) => s['name']['value']))

    return await this.gamesService.getGameById(id)
  }

  @ResolveField(() => [Achievement], { name: 'achievements' })
  public async getAchievements(@Parent() game: Game, @Args() args: AchievementsArgs) {
    return await this.achievementsService.getAchievementsByGameId(game.id, args)
  }

  @Mutation(() => Game, { name: 'createGame' })
  public async createGame(@Args('input') input: CreateGameInput) {
    return await this.gamesService.createGame(input)
  }
}
