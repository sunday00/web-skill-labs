import { useCallback, useState } from 'react'

export type ErrorAt = {
  // form field name
  // indicate for the input error
  at: string

  // locale string
  // this for translating other language
  lo: string
}

export const ERROR_MESSAGE: {
  [k: string]: ErrorAt
} = {
  duplicatedEmail: {
    at: 'email',
    lo: '이미 가입된 이메일입니다.',
  },
  tooShortPassword: {
    at: 'password',
    lo: '패스워드는 4글자 이상이어야 합니다.',
  },
}

export const useFormError = <T>(initial: T) => {
  const [formError, setFormError] = useState<T>(initial)

  const setLocaleError = useCallback((errorKey: keyof typeof ERROR_MESSAGE) => {
    const locale = ERROR_MESSAGE[errorKey]

    setFormError((f) => ({
      ...f,
      [locale.at]: locale.lo,
    }))
  }, [])

  return [formError, setLocaleError] as [T, (errorKey: keyof typeof ERROR_MESSAGE) => void]
}
