const STATUS = {
  PENDING: 'Request is pending.',
  FAILED: 'Request failed.',
  SUCCESS: 'Request was successful',
} as const

// type StatusWithResult = typeof STATUS extends { [K in keyof typeof STATUS]: infer V } ? V : never
type StatusWithResult = (typeof STATUS)[keyof typeof STATUS]

const s: StatusWithResult = 'Request is pending.'

export {}
