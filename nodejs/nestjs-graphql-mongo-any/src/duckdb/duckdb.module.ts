import { Module } from '@nestjs/common';
import { DuckdbService } from './duckdb.service';
import { DuckdbResolver } from './duckdb.resolver';

@Module({
  providers: [DuckdbResolver, DuckdbService],
})
export class DuckdbModule {}
