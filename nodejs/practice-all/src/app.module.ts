import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { AppStoreModule } from './app-store/app-store.module';
import { B64Module } from './b64/b64.module';
import { ServeStaticModule } from '@nestjs/serve-static';
import { join } from 'path';
import { DayjsModule } from './dayjs/dayjs.module';
import { CheckableModule } from './checkable/checkable.module';

@Module({
  imports: [
    AppStoreModule,
    B64Module,
    ServeStaticModule.forRoot({
      rootPath: join(__dirname, '..', 'client', 'dist'),
    }),
    DayjsModule,
    CheckableModule,
  ],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
