import { Controller, Get } from '@nestjs/common';
import { AppStoreService } from './app-store.service';

@Controller('app-store')
export class AppStoreController {
  constructor(private service: AppStoreService) {}

  @Get('/req-test-noti')
  async reqTestNoti() {
    return this.service.reqTestNoti();
  }

  @Get('/login')
  async login() {
    return this.service.login();
  }
}
