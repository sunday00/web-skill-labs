export const api = async (url: string, options: RequestInit) => {
  const res = await fetch(url, {
    ...options,
    headers: {
      'Content-Type': 'application/json',
      Authorization: `bearer ${localStorage.getItem('accessToken') ?? ''}`,
      ...options.headers,
    },
  })

  if (res.status < 400) return [true, await res.json()]

  const json = await res.json()
  console.error(json)

  return [false, json]
}
