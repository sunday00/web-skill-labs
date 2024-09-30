import { Controller, Get, Query } from '@nestjs/common';
import { CheckableService } from './checkable.service';

@Controller('checkable')
export class CheckableController {
  constructor(private readonly checkableService: CheckableService) {}

  @Get('/di')
  public getPoint(@Query('argument') repositoryName: string) {
    return this.checkableService.getPoint(repositoryName);
  }
}
