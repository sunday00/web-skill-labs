import { useCallback, useEffect, useState } from 'react'
import { CommonRes } from '@/common/common.entity'
import { useActionData, useLoaderData, useNavigate } from '@remix-run/react'
import { useToast } from '@/hooks/useToast'
import { ERROR_MESSAGE } from '@/hooks/error.message'

export function useRefreshableLoad<T>() {
  const [checked, setChecked] = useState(false)
  const { addAlert } = useToast()
  const navigate = useNavigate()

  const data: CommonRes<T> = JSON.parse(useLoaderData())

  const cb = useCallback(() => {
    if (data.statusCode === 401 && !checked) {
      setChecked(true)

      addAlert({ title: ERROR_MESSAGE['needToLogin'].lo, status: 'error', duration: 5 })
      navigate('/auth/signin')

      return
    }
  }, [addAlert, data.statusCode, checked, navigate])

  useEffect(() => {
    cb()
  }, [cb])

  return data
}

export function useRefreshableAction<T>() {
  const [checked, setChecked] = useState(false)
  const { addAlert } = useToast()
  const navigate = useNavigate()

  const data: CommonRes<T> = JSON.parse(useActionData() ?? '{}')

  const cb = useCallback(() => {
    if (data.statusCode === 401 && !checked) {
      setChecked(true)

      addAlert({ title: ERROR_MESSAGE['needToLogin'].lo, status: 'error', duration: 5 })
      navigate('/auth/signin')

      return
    }
  }, [addAlert, data.statusCode, checked, navigate])

  useEffect(() => {
    cb()
  }, [cb])

  return data
}
