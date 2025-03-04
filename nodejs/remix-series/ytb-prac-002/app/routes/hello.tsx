import { Outlet } from '@remix-run/react'

export default function hello() {
  return <>
    <h1>Wow!</h1>
    <Outlet />
  </>
}