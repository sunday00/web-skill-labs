import { useParams } from '@remix-run/react'

export default function HelloDetail() {
  const params = useParams()

  return (
    <>
      <h1>one! {params.id}</h1>
    </>
  )
}
