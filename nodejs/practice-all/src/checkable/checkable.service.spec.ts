import { Test, TestingModule } from '@nestjs/testing';
import { CheckableService } from './checkable.service';

describe('CheckableService', () => {
  let service: CheckableService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [CheckableService],
    }).compile();

    service = module.get<CheckableService>(CheckableService);
  });

  it('should be defined', () => {
    expect(service).toBeDefined();
  });
});
