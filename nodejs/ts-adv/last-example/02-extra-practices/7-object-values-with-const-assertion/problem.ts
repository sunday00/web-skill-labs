const STATUS = {
  PENDING: 'Request is pending.',
  FAILED: 'Request failed.',
  SUCCESS: 'Request was successful',
} as const

// type Status = (typeof STATUS)[Exclude<keyof typeof STATUS, 'PENDING'>]
type Status = (typeof STATUS)['SUCCESS' | 'FAILED']

export {}
