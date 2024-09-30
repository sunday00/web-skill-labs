import { Controller, Get } from '@nestjs/common';
import { DayjsService } from './dayjs.service';
import dayjs from 'dayjs';
import { ApiTags } from '@nestjs/swagger';
import { randPastDate } from '@ngneat/falso';

@ApiTags('dayjs')
@Controller('dayjs')
export class DayjsController {
  constructor(private readonly service: DayjsService) {}

  @Get('/')
  public async parse() {
    const dts = randPastDate({ length: 2000 });

    const st = dayjs();
    const result = dts
      .sort((a, b) => {
        return dayjs(a).isAfter(b) ? -1 : 1;
      })
      .map((dt) => ({
        dt: dayjs(dt).format('YYYYMMDD-HHmmss'),
        isNew: dayjs(dt).isAfter(dayjs().subtract(6, 'M')),
      }));
    const et = dayjs();

    console.log(et.diff(st, 'ms'));

    return result;
  }
}
