import { time } from '@/utils/time'

export type CommonListPage<T> = {
  items: T[]
  page: number
  size: number
  total: number
  lastPage: number
  nextPage: number
}

export type CommonSuccess<T> = {
  statusCode: number
  message: string
  data: T & { refreshToken?: string }
}

export type CommonError = {
  statusCode: number
  timestamp: string
  errorData: {
    path: string
    error: string
    refreshToken?: string
  }
}

export type CommonRes<T> = CommonSuccess<T> | CommonError

export const ManualError = ({ statusCode, message }: { statusCode: number; message?: string }) => {
  return {
    statusCode,
    message,
    errorData: { error: message ?? '', path: '' },
    timestamp: time().toISOString(),
  }
}
