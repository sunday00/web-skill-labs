import { useCallback, useEffect, useState } from 'react'
import { CommonRes } from '@/common/common.entity'
import { useActionData, useLoaderData, useNavigate } from '@remix-run/react'
import { loader } from '@/routes/articles/route'
import { useToast } from '@/hooks/useToast'

export function useRefreshableLoad<T>() {
  const [checked, setChecked] = useState(false)
  const { addAlert } = useToast()
  const navigate = useNavigate()

  const data: CommonRes<T> = JSON.parse(useLoaderData<typeof loader>())

  const load = useCallback(() => {
    if (data.statusCode === 401 && !checked) {
      setChecked(true)

      addAlert({ title: 'needToLogin', status: 'error', duration: 5 })
      navigate('/auth/signin')

      return
    }
  }, [addAlert, data.statusCode, checked, navigate])

  useEffect(() => {
    load()
  }, [load])

  return data
}

export function useRefreshableAction<T>() {
  const [posted, setPosted] = useState(false)
  const { addAlert } = useToast()
  const navigate = useNavigate()

  const data: CommonRes<T> = JSON.parse(useActionData<typeof action>() ?? '{}')

  const action = useCallback(() => {
    if (data.statusCode === 401 && !posted) {
      setPosted(true)

      addAlert({ title: 'needToLogin', status: 'error', duration: 5 })
      navigate('/auth/signin')

      return
    }
  }, [addAlert, data.statusCode, posted, navigate])

  useEffect(() => {
    action()
  }, [action])

  return data
}
