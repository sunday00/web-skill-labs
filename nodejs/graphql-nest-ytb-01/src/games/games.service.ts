import { Injectable } from '@nestjs/common'
import { InjectModel } from '@nestjs/mongoose'
import { GameEntity } from './schemas/game.schemas'
import { Model } from 'mongoose'
import { CreateGameInput } from './types/game.input.create'

@Injectable()
export class GamesService {
  constructor(
    @InjectModel(GameEntity.name)
    private gameModel: Model<GameEntity>,
  ) {}

  public async getGames(offset: number, limit: number) {
    return this.gameModel.find().skip(offset).limit(limit)
  }

  public async getGameById(id: string) {
    // return this.gameModel.findOne({ _id: new mongoose.Types.ObjectId(id) })
    return this.gameModel.findById(id)
  }

  async createGame(input: CreateGameInput) {
    return this.gameModel.create(input)
  }
}
