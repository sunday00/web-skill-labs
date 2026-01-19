import { Injectable } from '@nestjs/common'
import { CreateMovieInput } from '@/duckdb/struct/movie.entity'
import { DuckDBInstance } from '@duckdb/node-api'
import path from 'node:path'

@Injectable()
export class DuckdbService {
  async storeMovie(input: CreateMovieInput) {
    const instance = await DuckDBInstance.fromCache(
      path.join(process.cwd(), 'db', 'ducks', 'duck.parquet'),
      { threads: '4' },
    )

    const conn = await instance.connect()

    return await conn.run(`
      CREATE TABLE IF NOT EXISTS movies
      (
        id         INTEGER PRIMARY KEY,
        title      VARCHAR,
        director   VARCHAR
      );

      CREATE SEQUENCE IF NOT EXISTS seq_movieid START 1;

      INSERT INTO movies VALUES 
                           (nextval('seq_movieid'), 'Old Boy', 'Park chan-wook'), 
                           (nextval('seq_movieid'), 'Parasite', 'Bong Jun-ho')
        ;
    `)
  }
}
