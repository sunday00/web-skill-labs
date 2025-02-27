import type { MetaFunction } from '@remix-run/node'
import Box from '~/routes/components/Box'

export const meta: MetaFunction = () => {
  return [{ title: 'New Remix App' }, { name: 'description', content: 'Welcome to Remix!' }]
}

export default function Index() {
  return (
    <div className="container">
      {/*<Flex h={'dvh'} justify={'center'} align={'center'}>*/}
      <div className={'h-dvh w-dvw flex justify-center items-center'}>
        <Box gap={4}>
          <div className={'form-control w-full max-w-xs'}>
            <label htmlFor="name" className={'label label-text'}>
              name
            </label>
            <input
              type="text"
              name={'name'}
              placeholder="Type here"
              className="input input-bordered w-full max-w-xs"
              id={'name'}
            />
          </div>

          <div className={'form-control w-full max-w-xs'}>
            <label htmlFor="email" className={'label label-text'}>
              email
            </label>
            <input
              type="email"
              name={'email'}
              placeholder="Type here"
              className="input input-bordered w-full max-w-xs"
              id={'name'}
            />
          </div>
        </Box>
        {/*</Flex>*/}
      </div>
    </div>
  )
}
