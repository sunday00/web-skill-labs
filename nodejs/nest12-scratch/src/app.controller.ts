import { Controller, Get, Query } from '@nestjs/common'
import { AppService } from './app.service.js'

@Controller()
export class AppController {
  constructor(private readonly appService: AppService) {}

  @Get()
  getHello(): string {
    return this.appService.getHello()
  }

  @Get('/practice')
  practice(@Query() q: any): string {
    console.log(q)

    return '1'
  }
}
