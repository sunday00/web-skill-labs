import { Module } from '@nestjs/common';
import { DayjsController } from './dayjs.controller';
import { DayjsService } from './dayjs.service';

@Module({
  controllers: [DayjsController],
  providers: [DayjsService]
})
export class DayjsModule {}
