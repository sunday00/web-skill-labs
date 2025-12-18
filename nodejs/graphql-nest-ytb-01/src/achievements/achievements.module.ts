import { Module } from '@nestjs/common'
import { AchievementsService } from './achievements.service'
import { AchievementsResolver } from './achievements.resolver'
import { MongooseModule } from '@nestjs/mongoose'
import { AchievementEntity, AchievementSchema } from './schemas/achievement.schemas'

@Module({
  imports: [
    MongooseModule.forFeature([{ name: AchievementEntity.name, schema: AchievementSchema }]),
  ],
  providers: [AchievementsResolver, AchievementsService],
  exports: [AchievementsService],
})
export class AchievementsModule {}
