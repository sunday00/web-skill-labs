import { GithubLib } from './gitlib.interface';

export enum GHIssueState {
  OPEN,
  CLOSED,
}

export class Repository {
  private name: string;

  constructor(name: string) {
    this.name = name;
  }

  public hasIssues() {
    return true;
  }

  public getReadMe() {
    return '# Introduction. \n ## install. blah..';
  }

  public getPullRequest(state: GHIssueState) {
    return ['hey', 'it', 'bug'];
  }

  public getStargazersCount() {
    return 3;
  }

  public getForksCount() {
    return 7;
  }
}

export class PretendGithubLib {
  public static connect(): PretendGithubLib {
    return new PretendGithubLib();
  }

  public getRepository(repositoryName: string) {
    return new Repository(repositoryName);
  }
}
