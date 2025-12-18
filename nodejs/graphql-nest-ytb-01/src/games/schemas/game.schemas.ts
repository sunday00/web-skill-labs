import { Prop, Schema, SchemaFactory } from '@nestjs/mongoose'
import { HydratedDocument } from 'mongoose'

export type GameDocument = HydratedDocument<GameEntity>

@Schema({ collection: 'games' })
export class GameEntity {
  @Prop({ required: true })
  name: string

  @Prop({ required: true })
  genre: string

  @Prop()
  price: number
}

export const GameSchema = SchemaFactory.createForClass(GameEntity)
