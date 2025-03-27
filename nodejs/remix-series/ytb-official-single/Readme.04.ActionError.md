# Action & Validate

## input component

```typescript jsx
import Input from '@/components/form/input'

export default function Signup() {
  return (
    <Form method={'post'}>
      <Input
        label={'email'}
        name={'email'}
        type={'email'}
        autoComplete={'username'}
        icon={<FaEnvelope />}
        placeholder={'abc@def.com'}
        required={true}
        errorMessage={formError.email}
      />
    </Form>
  )
}
```

- 기본적 input 에 label, input 설명, error를 포함.
- input 모양 일관성을 위한 컴포넌트

## formData util

```typescript 
import { formData } from '@/utils/form.data'

export const action: ActionFunction = async ({ request }) => {
  const fd = await formData(request)

  const payload = {
    email: fd.get<string>('email'),
    name: fd.get<string>('name'),
    password: fd.get<string>('password'),
  }
}
```

- remix 기본 request.formData() 의 wrapper.
- 별건 아닌데, formData.get('something') 했을때 타입 불명확한거 귀찮아서 제네릭으로 넘기는 wrapper 사용.
- 필수는 아니고 적절히 응용

## form error state

```typescript jsx
import { useFormError } from '@/hooks/error.message'

export default function Signup() {
  const actionRes = useActionData<CommonRes<unknown>>()
  // use this like useState
  const [formError, setFormError] = useFormError({ email: '', password: '', name: '' })

  useEffect(() => {
    if (actionRes && 'data' in actionRes) {
      console.log('success') // anytihing ... ex) redirect
    } else if (actionRes?.errorData) {
      setFormError(actionRes.errorData.error)
    }
  }, [actionRes, setFormError])

  return (<section>
      <Form>
        <Input
          label={'name'}
          name={'name'}
          type={'name'}
          autoComplete={'name'}
          icon={<FaUser />}
          placeholder={'doctor_spider'}
          required={true}
          errorMessage={formError.name}
        />
      </Form>
    </section>
  )
}
```

- action 에서 에러 발생시 나오는 key message 를 사용자에 노출시 지정된 한글로 노출하도록 미리 정의
- /app/hooks/error.message.ts / ERROR_MESSAGE 참고 및 update 필요