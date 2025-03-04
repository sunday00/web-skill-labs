import { Form, useLoaderData, useParams } from '@remix-run/react'
import { LoaderFunctionArgs } from '@remix-run/node'
import { db } from '../../backends/api'
import { ReactNode } from 'react'

type Comment = {
  movieId: number
  id: number
  createdAt: string
  message: string
}

export const loader = async ({ params }: LoaderFunctionArgs) => {
  const raw = await db({ where: { movieId: params.id! }, orderBy: { createdAt: 'desc' } })

  return raw ?? []
}

export default function MovieIdComments() {
  const { id } = useParams()

  const commentData = useLoaderData<Comment[]>()
  const comments = commentData.map((comment: Comment) => {
    return (
      <div key={comment.id}>
        <p>{comment.message}</p>
      </div>
    )
  })

  return (
    <div className={'rounded-lg border p-3'}>
      <h1 className={'text-xl font-semibold mb-5'}>Your Opinion</h1>

      <div>
        <Form method={'POST'}>
          <input type="hidden" name={'id'} value={id} />
          <textarea
            name="comment"
            id="comment"
            className={'textarea textarea-neutral bg-base-100 w-full border-accent rounded-lg p-2'}
          ></textarea>

          <div className={'flex justify-end'}>
            <input type={'submit'} value={'ok'} className={'btn btn-outline btn-info btn-sm'} />
          </div>
        </Form>

        <div className={'mt-5 flex flex-col gap-y-3'}>{comments as ReactNode}</div>
      </div>
    </div>
  )
}
