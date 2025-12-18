import { Prop, Schema, SchemaFactory } from '@nestjs/mongoose'
import { HydratedDocument, Types } from 'mongoose'
import { Difficulty } from '../types/achievement.type'

export type AchievementDocument = HydratedDocument<AchievementEntity>

@Schema({ collection: 'achievements' })
export class AchievementEntity {
  @Prop({ required: true })
  title: string

  @Prop({ required: true })
  description: string

  @Prop({ enum: Difficulty })
  difficulty: Difficulty

  @Prop({ required: true, type: Number })
  points: number

  @Prop({ type: Types.ObjectId })
  gameId: Types.ObjectId
}

export const AchievementSchema = SchemaFactory.createForClass(AchievementEntity)
