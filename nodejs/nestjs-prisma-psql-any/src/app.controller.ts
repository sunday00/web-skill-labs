import { Controller, Get } from '@nestjs/common';
import { AppService } from './app.service';
import csv from 'csvtojson/v2';

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get()
  getHello(): string {
    return this.appService.getHello();
  }

  @Get('/csv')
  async testCsvToJson() {
    const j = await csv().fromFile(
      process.cwd() + '/storage/20230605002720-answers.csv',
    );
    console.log(j);
  }
}
