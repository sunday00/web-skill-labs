import { Form, useActionData, useLoaderData, useNavigation, useParams } from '@remix-run/react'
import { ActionFunctionArgs, LoaderFunctionArgs } from '@remix-run/node'
import { db } from '../../backends/api'
import { ReactNode, useEffect, useState } from 'react'

type Comment = {
  movieId: number
  id: number
  createdAt: string
  message: string
}

export const loader = async ({ params }: LoaderFunctionArgs) => {
  const raw = await db.select({ where: { movieId: params.id! }, orderBy: { createdAt: 'desc' } })

  return raw ?? []
}

export const action = async ({ request }: ActionFunctionArgs) => {
  const fd = await request.formData()

  const _result = db.create({
    movieId: Number(fd.get('id') ?? 0),
    message: fd.get('comment') as string,
  })

  return { statusCode: 200 }
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

  const navigation = useNavigation()

  const [comment, setComment] = useState('')
  const afterAction = useActionData<{ statusCode: number }>()

  useEffect(() => {
    if (afterAction?.statusCode === 200) {
      setComment('')
    }
  }, [afterAction])

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
            value={comment}
            onChange={(e) => setComment(e.target.value)}
          ></textarea>

          <div className={'flex justify-end'}>
            {navigation.state === 'submitting' ? (
              <input
                type={'button'}
                disabled
                value={'loading'}
                className={'btn btn-outline btn-info btn-sm'}
              />
            ) : (
              <input type={'submit'} value={'ok'} className={'btn btn-outline btn-info btn-sm'} />
            )}
          </div>
        </Form>

        <div className={'mt-5 flex flex-col gap-y-3'}>{comments as ReactNode}</div>
      </div>
    </div>
  )
}
