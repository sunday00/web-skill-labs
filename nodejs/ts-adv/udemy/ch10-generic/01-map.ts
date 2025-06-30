const submitData = <T extends { id: number; title: string }>(data: T) => {
  return {
    ...data,
    timestamp: new Date().getTime(),
  }
}

const data = [
  {
    id: 123,
    title: 'hello title',
  },
]

const submittedUsers = data.map(submitData)
