import { Module } from '@nestjs/common';
import { CheckableController } from './checkable.controller';
import { CheckableService } from './checkable.service';
import { GITHUBLIB_PROVIDER, GitlibAdapter } from './gitlib.adapter';
import { GithubLib } from './gitlib.interface';

@Module({
  controllers: [CheckableController],
  providers: [
    CheckableService,
    GitlibAdapter,
    {
      provide: GITHUBLIB_PROVIDER,
      useValue: new GitlibAdapter(),
    },
  ],
})
export class CheckableModule {}
