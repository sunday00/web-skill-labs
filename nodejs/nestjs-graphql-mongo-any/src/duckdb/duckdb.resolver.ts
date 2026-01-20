import { Args, Mutation, Resolver } from '@nestjs/graphql'
import { DuckdbService } from './duckdb.service'
import { CreateMovieInput, Movie } from '@/duckdb/struct/movie.entity'

@Resolver()
export class DuckdbResolver {
  constructor(private readonly duckdbService: DuckdbService) {}

  @Mutation(() => Movie, { name: 'createMovie' })
  public async storeMovie(@Args('input') input: CreateMovieInput) {
    const r = await this.duckdbService.storeMovie(input)

    console.log(r)

    return {
      id: 1,
      title: input.title,
      director: input.director,
    }
  }
}
