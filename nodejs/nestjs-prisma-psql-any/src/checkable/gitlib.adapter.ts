import { Injectable } from '@nestjs/common';
import { GithubLib } from './gitlib.interface';
import { PretendGithubLib } from './pretend.github.lib';

export const GITHUBLIB_PROVIDER = 'GITHUBLIB_PROVIDER';

@Injectable()
export class GitlibAdapter implements GithubLib {
  connect() {
    return new PretendGithubLib();
  }
}
