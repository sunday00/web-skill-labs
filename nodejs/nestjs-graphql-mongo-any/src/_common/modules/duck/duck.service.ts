import { Injectable } from '@nestjs/common'
import { DuckDBConnection, DuckDBInstance } from '@duckdb/node-api'
import path from 'node:path'

@Injectable()
export class DuckService {
  public instance: DuckDBInstance
  public conn: DuckDBConnection

  constructor() {}

  public async init() {
    this.instance = await DuckDBInstance.fromCache(
      path.join(process.cwd(), 'db', 'ducks', 'duck.db'),
      { threads: '4' },
    )

    this.conn = await this.instance.connect()
  }
}
