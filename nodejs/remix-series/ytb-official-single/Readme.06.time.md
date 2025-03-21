# date / time

- app/utils/time.ts
- dayjs wrapper

```typescript jsx
import { time } from '@/utils/time'

export default function ArticleDetail() {
  return (<div>{time(article?.createdAt).fromNow()}</div>)
}
```

- 시간, 날짜 util 은 교체될 수 있어서, 되도록 wrapper 사용 권장.