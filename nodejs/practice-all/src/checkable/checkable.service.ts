import { Inject, Injectable } from '@nestjs/common';
import { GHIssueState, PretendGithubLib } from './pretend.github.lib';
import { GITHUBLIB_PROVIDER, GitlibAdapter } from './gitlib.adapter';
import { GithubLib } from './gitlib.interface';

@Injectable()
export class CheckableService {
  constructor(
    @Inject(GITHUBLIB_PROVIDER) private readonly githubLib: GithubLib,
  ) {}

  public getPoint(repositoryName: string) {
    // const gitHup = PretendGithubLib.connect();
    const gitHup = this.githubLib.connect();

    const repository = gitHup.getRepository(repositoryName);

    let points = 0;
    if (repository.hasIssues()) points += 1;
    if (repository.getReadMe()) points += 1;
    if (repository.getPullRequest(GHIssueState.CLOSED).length) points += 1;

    points += repository.getStargazersCount();
    points += repository.getForksCount();

    return points;
  }
}
