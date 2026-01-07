import { Injectable } from '@nestjs/common';
import { PretendGithubLib } from './pretend.github.lib';

export interface GithubLib {
  connect(): PretendGithubLib;
}
