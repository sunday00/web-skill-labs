# Request / Response

## request with refresh

### in loader

```typescript
import { METHOD, refreshableFetch } from '@/common/refreshable.fetch'

export const loader: LoaderFunction = async ({ request }) => {
  // make url
  const url = `/board`
  const qs = new URL(request.url).searchParams

  // use refreshableFetch function
  return refreshableFetch({
    request,
    method: METHOD.GET, // use enum
    url,
    data: { page: Number(qs.get('page') ?? 1), size: Number(qs.get('size') ?? 20) }, // just set object
  })
}
```

- `@/common/refreshable.fetch` 에서 import

#### refreshableFetch

1. 기능
    - auth cookie 추출해서 헤더에 자동 입력 후 requesting
    - 해당 쿠키 만료시 자동 refresh 요청
    - 응답에 refreshed 갱신된 쿠키 자동 저장
    - get 이면 data 를 queryString 으로, post 면 body stringify 해서 요청함

2. 사용
    - 상기 기능을 이유로 loderFunction 기본 request,
    - MethodEnum
    - data에 get/post 구분 없이 object 넣으면 됨.

### in client Component

```typescript jsx
import { useRefreshableLoad } from '@/hooks/use.refreshable'

export default function Articles() {
  // use useRefreshableLoad function
  const raw = useRefreshableLoad<CommonListPage<Article>>()
  // raw { data: { items: Article[], page, size, total, nextPage... } }
  // or
  // raw { statusCode: 000, errorData: { error: '', message: '', } }

  // handle error

  const articles = raw.data

  const list = articles.items.map((article: Article) => {
    return <ArticleListItem key={article.id} entity={article} />
  })

  return (
    <section>
      <ul>{list as ReactNode}</ul>
    </section>
  )
}
```

#### useRefreshableLoad

1. 기능
    - return string JSON parse
        - refreshableFetch 에서 쿠키 재저장 해더 사용 때문에 응답이 raw string 으로 return
    - 쿠키 없거나 refresh 까지 만료되어 쿠키 자동 재발급이 불가능한 경우 로그인 페이지로 redirect
    - redirect 되면서 toasting
2. 사용
    - refreshableFetch 와 함께 사용.
    - jwt 토큰 만료시에는 자동으로 리다이렉트 되므로, guest 에 허용된 페이지에서는 remix builtIn `useLoaderData` 함수를 사용할 것.

#### useRefreshableAction

1. 내부적으로 useActionData wrpping 함수이고 useRefreshableLoad 와 차이 없음.
2. `export const action: ActionFunction = async ({ request }) => {}` 와 함께 사용.

