import { Injectable } from '@nestjs/common'
import { InjectModel } from '@nestjs/mongoose'
import { GameEntity } from './schemas/game.schemas'
import { Model } from 'mongoose'

@Injectable()
export class GamesService {
  constructor(
    @InjectModel(GameEntity.name)
    private gameModel: Model<GameEntity>,
  ) {}

  public async getGames() {
    return this.gameModel.find()
  }
}
