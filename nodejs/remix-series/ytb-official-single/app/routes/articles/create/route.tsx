import Box from '@/components/layouts/box'
import Fieldset from '@/components/form/fieldset'
import Input from '@/components/form/input'
import Button from '@/components/form/button'
import { useEffect, useRef } from 'react'
import { Form, useNavigate, useNavigation } from '@remix-run/react'
import { ActionFunction } from '@remix-run/node'
import { METHOD, refreshableFetch } from '@/common/refreshable.fetch'
import { useRefreshableAction } from '@/hooks/use.refreshable'
import { Article } from '@/entities.extends/board.extend.entity'
import { routes } from '@/common/routes'

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

export default function ArticleCreate() {
  const loading = useNavigation()
  const formRef = useRef<HTMLFormElement>(null)
  const navigate = useNavigate()

  const action = useRefreshableAction<Article>()

  useEffect(() => {
    // if (action && loading.state === 'idle') {
    //   formRef.current?.reset()
    //   formRef.current?.querySelector<HTMLInputElement>('[name="title"]')?.focus()
    // }

    if (action && loading.state === 'idle' && 'data' in action)
      navigate(routes.articles.$id.path.replace(':id', action.data?.id.toString()), {
        replace: true,
      })
  }, [action, loading.state, navigate])

  return (
    <section>
      <Form method={'post'} className={'bg-base-200 p-4'} ref={formRef}>
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
            text={'Write'}
            className={'mx-auto'}
            name={'_action'}
            value={'createArticle'}
            pending={
              loading.state === 'submitting' && loading.formData?.get('_action') === 'createArticle'
            }
          />
        </Box>
      </Form>
    </section>
  )
}
