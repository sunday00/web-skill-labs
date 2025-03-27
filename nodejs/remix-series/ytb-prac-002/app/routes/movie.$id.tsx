import { LoaderFunctionArgs } from '@remix-run/node'
import { Link, Outlet, useLoaderData } from '@remix-run/react'

type Movie = {
  adult: false
  backdrop_path: string
  belongs_to_collection?: unknown
  budget: number
  genres: { id: number; name: string }[]
  homepage: string
  id: number
  imdb_id: string
  origin_country: string[]
  original_language: string
  original_title: string
  overview: string
  popularity: number
  poster_path: string
  production_companies: { id: number; logo_path: string; name: string; origin_country: string }[]
  production_countries: { iso_3166_1: string; name: string }[]
  release_date: string
  revenue: number
  runtime: number
  spoken_languages: { english_name: string; iso_639_1: string; name: string }[]
  status: string
  tagline: string
  title: string
  video: false
  vote_average: number
  vote_count: number
}

export const loader = async ({ params }: LoaderFunctionArgs) => {
  const url = `https://api.themoviedb.org/3/movie/${params.id}?language=ko-KR`
  const options = {
    method: 'GET',
    headers: {
      accept: 'application/json',
      Authorization: 'Bearer .......',
    },
  }

  return fetch(url, options)
    .then((res) => res.json())
    .catch((err) => console.error(err))
}

export default function MovieId() {
  const movie = useLoaderData<Movie>()

  return (
    <div className={'min-h-screen p-10'}>
      <img
        src={`https://image.tmdb.org/t/p/original/${movie.poster_path}`}
        alt={`${movie.original_title} thumb`}
        className={'h-[40vh] object-cover w-full rounded-lg'}
      />

      <h1 className={'text-4xl font-bold text-center pt-5'}>{movie.title}</h1>

      <div className={'flex gap-x-10 mt-10'}>
        <div className={'w-1/2 font-medium'}>
          <h1>
            <Link to={movie.homepage} target={'_blank'} rel="noreferrer">
              <span className={'underline'}>homepage:{movie.homepage}</span>
            </Link>
          </h1>

          <h1>
            <span className={'underline'}>Original Language</span>
            <span> : </span>
            {movie.original_language}
          </h1>

          <p>
            <span className={'underline'}>Overview</span>
            <span> : </span>
            {movie.overview}
          </p>

          <p>
            <span className={'underline'}>Release Date</span>
            <span> : </span>
            {movie.release_date}
          </p>
        </div>

        <div className={'w-1/2 font-medium'}>
          <Outlet />
        </div>
      </div>
    </div>
  )
}
