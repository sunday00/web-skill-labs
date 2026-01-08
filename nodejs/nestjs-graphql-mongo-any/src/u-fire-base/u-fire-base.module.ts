import { Module } from '@nestjs/common'
import { UFireBaseService } from './u-fire-base.service'
import { UFireBaseResolver } from './u-fire-base.resolver'

@Module({
  providers: [UFireBaseResolver, UFireBaseService],
})
export class UFireBaseModule {}
