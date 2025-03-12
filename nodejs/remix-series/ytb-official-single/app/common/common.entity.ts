export type CommonListPage<T> = {
  items: T[]
  page: number
  size: number
  total: number
  lastPage: number
  nextPage: number
  refreshToken?: string
}

export type CommonSuccess<T> = {
  statusCode: number
  message: string
  data: T
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
