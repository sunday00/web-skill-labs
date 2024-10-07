export const getFooBar = async () => {
  const result = await fetch(
    'http://localhost:3004/api/v20231013/fooBarThing/category/vue2/sub/practice',
  )

  const users = await result.json()

  return users.items[0]?.foo
}

export const appendUser = async (users, user) => {
  const result = await fetch(
    'http://localhost:3004/api/v20231013/fooBarThing/1',
    {
      method: 'PUT',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        category: 'vue2',
        subtegory: 'practice',
        title: 'users',
        foo: [...users, { id: user.id, name: user.name }],
        bar: {},
      }),
    },
  )

  return result.status
}
