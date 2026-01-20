import { Injectable } from '@nestjs/common'
import { CreateMovieInput } from '@/duckdb/struct/movie.entity'
import { DuckService } from '@/_common/modules/duck/duck.service'
import { Result } from '@duckdb/node-bindings'

@Injectable()
export class DuckdbService {
  constructor(private readonly duckService: DuckService) {}

  async storeMovie(input: CreateMovieInput): Promise<Result> {
    const res = await this.duckService.conn.run(`
      CREATE SEQUENCE IF NOT EXISTS seq_movieid START 1;

      CREATE TABLE IF NOT EXISTS movies
      (
        id         INTEGER PRIMARY KEY DEFAULT nextval('seq_movieid'),
        title      VARCHAR,
        director   VARCHAR
      );

      INSERT INTO movies (title, director) VALUES ('${input.title}', '${input.director}') RETURNING id;
    `)

    // await this.duckService.conn.run(
    //   `COPY (SELECT * FROM movies) TO 'duck.parquet' (FORMAT 'parquet')`,
    // )

    return res['result']
  }
}
