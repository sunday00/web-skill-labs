import type { MetaFunction } from '@remix-run/node'

export const meta: MetaFunction = () => {
  return [{ title: 'New Remix App' }, { name: 'description', content: 'Welcome to Remix!' }]
}

export default function Index() {
  return (
    <div className="bg-white py-6 sm:py-8 lg:py-12">
      <div className={'mx-auto max-w-screen-2xl px-4 md:px-8'}>
        <div className={'mb-10 md:mb-16'}>
          <h2
            className={'mb-4 text-center text-2xl font-bold text-neutral-800 md:mb-6 lg:text-3xl'}
          >
            Top Trending Movies
          </h2>
        </div>
      </div>
    </div>
  )
}
