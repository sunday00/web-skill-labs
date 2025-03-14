import { ActionFunction, LoaderFunction } from '@remix-run/node'
import { Form, useActionData, useLoaderData, useNavigate, useNavigation } from '@remix-run/react'
import { CommonListPage, CommonRes, CommonSuccess } from '@/common/common.entity'
import { Article } from '@/entities/board.entity'
import { ReactNode, useEffect } from 'react'
import Fieldset from '@/components/form/fieldset'
import Box from '@/components/layouts/box'
import Input from '@/components/form/input'
import Button from '@/components/form/button'
import { getCookie } from '@/routes/auth/signin/cookie.manager'

export const loader: LoaderFunction = async ({ request }) => {
  const url = 'http://localhost:3031/api/v1/board?page=1&size=40'
  const accessToken = await getCookie('accessToken', request)

  if (!accessToken) {
    // TODO set Toasts
    return {
      statusCode: 401,
    }
  }

  const raw = await fetch(url, {
    method: 'GET',
    headers: {
      Accept: 'application/json',
      'Content-Type': 'application/json',
      Authorization: 'Bearer ' + accessToken,
    },
  })

  return raw.json()
}

export const action: ActionFunction = async ({ request }) => {
  const fd = await request.formData()

  const raw = await fetch(`http://localhost:3031/api/v1/board`, {
    method: 'POST',
    headers: {
      Accept: 'application/json',
      'Content-Type': 'application/json',
      Authorization:
        'Bearer ' +
        'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpZCI6IjYyOGRlYzY5LTZjZTctNDA4Ni1iNzk1LTkwNjMzNjNjMDg5ZiIsInJvbGUiOjQsIm5hbWUiOiJncmF5bWFuIiwiZW1haWwiOiJzdW5kYXkwMDAwQG5hdGUuY29tIiwiaWF0IjoxNzQxNzcyOTgzLCJleHAiOjE3NDE3ODM3ODN9.egB2Em6motVIecaLv8_uN5bc2kWOnDvLjVMnZes4RA8',
    },
    body: JSON.stringify({
      title: fd.get('title'),
      content: fd.get('content'),
      type: fd.get('type'),
      category: fd.get('category'),
    }),
  })

  return raw.json()
}

export default function Articles() {
  const articles = useLoaderData<CommonRes<CommonListPage<Article>>>()

  const raw = 'data' in articles ? articles.data.items : []
  const list = raw.map((article: Article) => {
    return <li key={article.id}>{article.title}</li>
  })

  const actionData = useActionData<CommonSuccess<unknown>>()

  const loading = useNavigation()
  const navigate = useNavigate()

  useEffect(() => {
    if (articles.statusCode === 401) {
      // TODO set toast
      navigate('/auth/signin')

      return
    }

    if (actionData) console.log(actionData?.statusCode)
  }, [actionData, actionData?.statusCode, articles.statusCode, global, navigate])

  return (
    <section className={''}>
      <h1>ARTICLES</h1>

      <Form method={'post'} className={'bg-base-200 p-4'}>
        {/*<Form reloadDocument method={'post'} className={'bg-base-200 p-4'}>*/}
        <input type="hidden" name={'type'} value={'APP_COMMUNITY'} />
        <input type="hidden" name={'category'} value={'USER_NORMAL'} />
        <Box>
          <Fieldset>
            <Input label={'title'} name={'title'} />
          </Fieldset>

          <Fieldset disabled={loading.state === 'submitting'}>
            <textarea
              className="textarea textarea-bordered w-full"
              name={'content'}
              placeholder="make something surprise"
              rows={5}
            ></textarea>
          </Fieldset>

          <Button
            type={'submit'}
            text={loading.state === 'submitting' ? 'loading......' : 'Write'}
            className={'mx-auto'}
            // disabled={loading.state === 'submitting'}
          />
        </Box>
      </Form>

      <ul>{list as ReactNode}</ul>
    </section>
  )
}
