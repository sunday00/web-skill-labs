import { Outlet } from '@remix-run/react'

export default function Auth() {
  return (
    <>
      <h1>Auth</h1>
      <Outlet />
    </>
  )
}
