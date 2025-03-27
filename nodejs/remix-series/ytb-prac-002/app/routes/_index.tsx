import type { LoaderFunctionArgs, MetaFunction } from '@remix-run/node'
import { Link, useLoaderData } from '@remix-run/react'
import { ReactNode } from 'react'

export const meta: MetaFunction = () => {
  return [{ title: 'New Remix App' }, { name: 'description', content: 'Welcome to Remix!' }]
}

type MovieItem = {
  backdrop_path: string
  id: number
  title: string
  original_title: string
  overview: string
  poster_path: string
  media_type: string
  adult: false
  original_language: string
  genre_ids: number[]
  popularity: number
  release_date: string
  video: false
  vote_average: number
  vote_count: number
}

export const loader = async (_: LoaderFunctionArgs) => {
  const url = 'https://api.themoviedb.org/3/trending/movie/day?language=en-US'
  const options = {
    method: 'GET',
    headers: {
      accept: 'application/json',
      Authorization: 'Bearer .....',
    },
  }

  return fetch(url, options)
    .then((res) => res.json())
    .catch((err) => console.error(err))
}

export default function Index() {
  const data = useLoaderData<{ results: MovieItem[] }>()

  const list = data.results.map((d) => {
    return (
      <div className={'flex flex-col overflow-hidden rounded-lg border'} key={d.id}>
        <Link
          prefetch={'intent'}
          to={`/movie/${d.id}/comments`}
          className={'group relative block h-48 overflow-hidden bg-base-100 md:h-64'}
        >
          <img
            src={`https://image.tmdb.org/t/p/w500/${d.poster_path}`}
            alt={`${d.title} thumb`}
            className={
              'absolute inset-0 h-full w-full object-cover object-center transition duration-200 group-hover:scale-110'
            }
          />
        </Link>

        <div className={'flex flex-1 flex-col p-4 sm:p-6'}>
          <h2 className={'mb-2 text-lg font-semibold'}>
            <Link
              prefetch={'intent'}
              to={`/movie/${d.id}/comments`}
              className={
                'transition duration-100 hover:text-accent hover:opacity-80 active:text-accent'
              }
            >
              {d.title}
            </Link>
          </h2>

          <p className={'text-base-content line-clamp-3'}>{d.overview}</p>
        </div>
      </div>
    )
  })

  return (
    <div className="py-6 sm:py-8 lg:py-12">
      <div className={'mx-auto max-w-screen-2xl px-4 md:px-8'}>
        <div className={'mb-10 md:mb-16'}>
          <h2 className={'mb-4 text-center text-2xl font-bold md:mb-6 lg:text-3xl'}>
            Top Trending Movies
          </h2>
        </div>

        <div
          className={'grid gap-4 sm:grid-cols-2 md:gap-6 lg:grid-cols-3 xl:grid-cols-4 xl:gap-8'}
        >
          {list as ReactNode}
        </div>
      </div>
    </div>
  )
}
