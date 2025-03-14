import { ActionFunction, LoaderFunction } from '@remix-run/node'
import { Form, useNavigation } from '@remix-run/react'
import { CommonListPage } from '@/common/common.entity'
import { Article } from '@/entities/board.entity'
import { ReactNode } from 'react'
import Fieldset from '@/components/form/fieldset'
import Box from '@/components/layouts/box'
import Input from '@/components/form/input'
import Button from '@/components/form/button'
import { METHOD, refreshableFetch } from '@/common/refreshable.fetch'
import { useRefreshableAction, useRefreshableLoad } from '@/hooks/use.refreshable'

export const loader: LoaderFunction = async ({ request }) => {
  const url = `/board`

  return refreshableFetch({ request, method: METHOD.GET, url, data: { page: 1, size: 10 } })
}

export const action: ActionFunction = async ({ request }) => {
  const fd = await request.formData()
  const url = `/board`

  return refreshableFetch({
    request,
    method: METHOD.POST,
    url,
    data: {
      title: fd.get('title'),
      content: fd.get('content'),
      type: fd.get('type'),
      category: fd.get('category'),
    },
  })
}

export default function Articles() {
  const loading = useNavigation()

  const articles = useRefreshableLoad<CommonListPage<Article>>()

  const raw = 'data' in articles ? articles.data.items : []
  const list = raw.map((article: Article) => {
    return <li key={article.id}>{article.title}</li>
  })

  const action = useRefreshableAction<Article>()

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
            name={'_action'}
            value={'createArticle'}
          />
        </Box>
      </Form>

      <ul>{list as ReactNode}</ul>
    </section>
  )
}
