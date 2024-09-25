'use server'

import {revalidateTag} from "next/cache";

const submitUser = async (name: string, age: number) => {
  const result = await fetch('http://127.0.0.1:3400/fetchTest', {
    method: 'POST',
    body: JSON.stringify({ name, age }),
  })

  if (await result.json()) {
    revalidateTag('fetchTest')
  }
}

export const handleCreateUser = async (formData: FormData) => {
    await submitUser(
      formData.get('name') as string,
      Number(formData.get('age')),
    )
}
