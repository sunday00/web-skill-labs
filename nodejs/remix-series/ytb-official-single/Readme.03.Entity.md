# Communicate Backend

## api list

- backend.dev host + `/docs`
    - swagger
- backend.dev host + `/docs-json`
    - postman 을 위한 openapi json

## core entity type

```shell
  pnpm g:ent
```

- `/[project]/dev/sdk.ts`
    - 해당하는 backend repository, branch 설정 후 실행
    - env 에 허가된 GIT HUB ID 로 발급받은 토큰 저장 필요 - private repository 접근
    - backend 의 typeorm entity 파일 변형하여 기본 타입 생성
    - app/entities 에 저장

## entity 확장

- backend의 자동 type을 받았으나 살짝 아쉬워 field 추가, 필수값 제거, field 변형 필요하면
    - /app/entities.extends 폴더에 저장 바람

```typescript
export type Article = A & { userName: string } // field 추가

export type ArticleWithOutUpdate = Omit<Article, 'updatedAt'> // field 제거

// ... usage
import { ArticleWithOutUpdate } from '@/entities.extends/board.extend.entity.ts'

const article: ArticleWithOutUpdate = await getSomeArticleForExample()

```

