# Route

## forder baseed

- 리믹스는 folder 기준으로 routing 이 next와 다름.
    - (기존) 한 폴더에 하나의 페이지가 구성되고, 폴더에는 컴포넌트 파일들로 구성
    - 이때문에 하나의 도메인 안에 3depth까지 여러 묶을 단위로 관리하기에는 좀 불편...
- 수동으로 vite.config.ts에서 일일이 route 지정 가능.
    - 근데 이것도 번거로워서...
    - vite 안에 folder 기반으로 routing 등록 되도록 해둠

## route 규칙

### routes 안에 폴더 생성

- routes 바로 아래에 폴더는 `http://host:port/{바로 여기}`에 해당
- route.tsx 생성 필요. 바로 아래에 route는 해당 도메인의 layout이 된다.

```typescript jsx
import { Outlet } from '@remix-run/react'

export default function ArticlesLayout() {
  return (
    <article className={''}>
      <Outlet />
    </article>
  )
}

```

- 그 아래에 폴더부터는 기본적으로 다음 주소 `http://host:port/first-child/{이젠 여기}`
- _ (언더바) 로 시작하는 폴더는 index처럼 주소값이 '' (빈문자열)를 담당하는 페이지이다.
    - ex) `.../article/create` , `.../article/edit` ... 처럼 `.../article/list` 라고 해도 되지만, restful 하게 `.../article` 에 list
      기능을 두고 싶은 경우
- $ (달러 사인)로 시작하는 폴더는 useParams 에 url parameter 로 잡힌다.
    - `.../article/$id` => `.../article/123` || `.../article/$slug` => `.../article/hey-yo-123-man`

- 구상으로는 이편이 나중에 관리하기 편할 거 같아서 일단 구현해 놓기는 했는데,
    - 실무에서는 폴더+routes / 폴더+layout route.tsx + outlet / 폴더 + route.tsx 이 셋트를 매번 구성하기 번거로워지면, 새로운 방안이 필요할 수 있다.

## app / common / routes.tsx

- vite에서 폴더를 구성하면서 모인 라우트 주소 목록은 자동으로 객체리터럴로 저장된다.
- Link to 등에서 수동으로 href 를 문자열로 하지 말고, 이걸 이용하면, 라우트가 바뀌거나 오타가 발생할 때 알기 쉽다.

```typescript jsx
<Link to={'artcle/abcd'}>글 읽기</Link> // X. article 에 오타. 시작을 /로 하지 않아 다른 주소에 tail될 수도 있음.

// 

import { routes } from '@/common/routes'

...
<Link to={routes.articles.$id.path.replace(':id', item.id.toString())}>글 읽기</Link> // 권장

```