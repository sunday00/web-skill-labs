export const formData = async (request: Request) => {
  const fd = await request.formData()

  return {
    ...fd,
    get<T>(key: string): T {
      return fd.get(key) as T
    },
  }
}
