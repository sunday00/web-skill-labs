import { Injectable } from '@nestjs/common'
import { InjectModel } from '@nestjs/mongoose'
import { AchievementEntity } from './schemas/achievement.schemas'
import mongoose, { Model } from 'mongoose'
import { AchievementsArgs } from '../games/types/acheivements.args'
import { CreateAchievementInput } from './types/achievement.input.create'
import { GameEntity } from '../games/schemas/game.schemas'

@Injectable()
export class AchievementsService {
  constructor(
    @InjectModel(AchievementEntity.name)
    private achieveModel: Model<AchievementEntity>,
    @InjectModel(GameEntity.name)
    private gameModel: Model<GameEntity>,
  ) {}

  public async getAchievementsByGameId(gameId: string, args: AchievementsArgs) {
    const match = { gameId: new mongoose.Types.ObjectId(gameId) }

    if (args.difficulty) {
      match['difficulty'] = args.difficulty
    }

    return this.achieveModel.find(match).skip(args.offset).limit(args.limit)
  }

  async createAchievement(input: CreateAchievementInput) {
    // return this.achieveModel.create(input)

    const { gameId, ...d } = input

    const game = await this.gameModel.findById(gameId)
    if (!game) throw new Error('no game found')

    const newAchievement = new this.achieveModel({
      ...d,
      gameId: new mongoose.Types.ObjectId(gameId),
    })

    return newAchievement.save()
  }
}
