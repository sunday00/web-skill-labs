import { Field, InputType, ObjectType } from '@nestjs/graphql'

@ObjectType()
export class Movie {
  @Field()
  id: number

  @Field()
  title: string

  @Field()
  director: string
}

@InputType()
export class CreateMovieInput implements Omit<Movie, 'id'> {
  @Field()
  title: string

  @Field()
  director: string
}
