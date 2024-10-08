import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { AppStoreModule } from './app-store/app-store.module';
import { B64Module } from './b64/b64.module';
import { ServeStaticModule } from '@nestjs/serve-static';
import { join } from 'path';
import { DayjsModule } from './dayjs/dayjs.module';
import { CheckableModule } from './checkable/checkable.module';
import { ArticleModule } from './article/article.module';
import { PrismaModule } from './common/prisma/prisma.module';
import { ConfigModule } from '@nestjs/config';
import { configModuleOption } from './common/config/app.config';

@Module({
  imports: [
    ConfigModule.forRoot(configModuleOption),
    PrismaModule,
    AppStoreModule,
    B64Module,
    ServeStaticModule.forRoot({
      rootPath: join(__dirname, '..', 'client', 'dist'),
    }),
    DayjsModule,
    CheckableModule,
    ArticleModule,
  ],
  controllers: [AppController],
  providers: [AppService],
})
export class AppModule {}
