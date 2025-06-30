export const getUser = async <T>(id: number) => {
  const user: T = await fetch(`"/users"/${id}`).then((response) => response.json())
  return user
}

/*
  Your job:
  Make the getUser functino accept generic type arguments so the data infers 
  the proper type that is
*/

const myFunc = async () => {
  const data = await getUser<{ id: number; username: string }>(23)
}
