import { LoaderFunction } from '@remix-run/node'
import { useLoaderData } from '@remix-run/react'

export type Article = {}

export const loader: LoaderFunction = async () => {
  const url = 'http://localhost:3031/api/v1/board?page=1&size=40'
  const raw = await fetch(url, {
    method: 'GET',
    headers: {
      Accept: 'application/json',
      'Content-Type': 'application/json',
      Authorization:
        'Bearer ' +
        'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6IjYyOGRlYzY5LTZjZTctNDA4Ni1iNzk1LTkwNjMzNjNjMDg5ZiIsInJvbGUiOjQsIm5hbWUiOiJncmF5bWFuIiwiZW1haWwiOiJzdW5kYXkwMDAwQG5hdGUuY29tIiwiaWF0IjoxNzQxNzQ3MzMwLCJleHAiOjE3NDE3NTgxMzB9.rvqnufDEu0lKH7GzHn1VOApevQxBLI611Zbcv9aJi70',
    },
  })

  return raw.json()
}

export default function Articles() {
  const articles = useLoaderData()

  const list = []

  return (
    <section className={''}>
      <h1>ARTICLES</h1>

      <ul></ul>
    </section>
  )
}
