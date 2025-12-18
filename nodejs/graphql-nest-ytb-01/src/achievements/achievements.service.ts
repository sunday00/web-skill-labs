import { Injectable } from '@nestjs/common'
import { InjectModel } from '@nestjs/mongoose'
import { AchievementEntity } from './schemas/achievement.schemas'
import mongoose, { Model } from 'mongoose'

@Injectable()
export class AchievementsService {
  constructor(
    @InjectModel(AchievementEntity.name)
    private achieveModel: Model<AchievementEntity>,
  ) {}

  public async getAchievementsByGameId(gameId: string) {
    return this.achieveModel.find({ gameId: new mongoose.Types.ObjectId(gameId) })
  }
}
