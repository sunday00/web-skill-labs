import { Form, Link, useLocation } from '@remix-run/react'
import { ReactNode } from 'react'
import { JWTPayload } from '@/routes/auth/signin/cookie.manager'
import { FaSignInAlt, FaSignOutAlt } from 'react-icons/fa'
import Button from '@/components/form/button'
import ThemeBtn from '@/components/func/theme.btn'
import { FaTwitter, FaUser } from 'react-icons/fa6'
import { routes } from '@/common/routes'
import { useEnv } from '@/hooks/useEnv'

const dirs = ['articles', 'newsletters', 'plain']

export default function Navigation({ user, token }: { user?: JWTPayload; token?: string }) {
  const location = useLocation()
  const pathname = location.pathname

  const menus = dirs.map((d) => {
    return (
      <li key={d} className={pathname === '/' + d ? 'font-bold' : ''}>
        <Link to={`/${d}`} preventScrollReset={true}>
          {d}
        </Link>
      </li>
    )
  })

  const env = useEnv()

  const handleShowDevInfo = () => {
    console.log('token: ', token)
    console.log(window.env)
    console.log({ payload: user })
  }

  return (
    <nav className="navbar bg-base-100 shadow-sm fixed top-0">
      <div className="navbar-start">
        <a href={'/'} className="btn btn-ghost text-xl">
          daisyUI
        </a>
      </div>

      <div className="navbar-center">
        <ul className="menu menu-horizontal px-1 text-lg">{menus as ReactNode}</ul>
      </div>

      <div className="navbar-end flex items-center gap-2">
        <div className={'auth-btn flex items-center'}>
          {user ? (
            <div className="dropdown dropdown-end">
              <Button
                type={'submit'}
                tabIndex={0}
                className={'btn p-0'}
                variant={'ghost'}
                w={'auto'}
              >
                <FaUser className="h-8 w-8" />
              </Button>
              <ul className="dropdown-content menu bg-base-100 rounded-box z-[1] w-52 p-2 shadow gap-2">
                <li>
                  <Link tabIndex={0} to={'/auth/my'} className={'btn'}>
                    My Profile
                  </Link>
                </li>
                <li>
                  <Form
                    name={'auth.logout'}
                    method={'post'}
                    action={routes.auth.signout.path}
                    className={'block p-0'}
                  >
                    <Button
                      type={'submit'}
                      variant={'ghost'}
                      w={'w-full'}
                      className={'hover:bg-transparent'}
                    >
                      <FaSignOutAlt />
                    </Button>
                  </Form>
                </li>
                {env['NODE_ENV'] === 'development' && (
                  <li>
                    <Button
                      onClick={handleShowDevInfo}
                      type={'button'}
                      variant={'ghost'}
                      w={'w-full'}
                    >
                      Show Dev Info
                    </Button>
                  </li>
                )}
              </ul>
            </div>
          ) : (
            <>
              <Link to={routes.auth.signin.path}>
                <FaSignInAlt className="h-8 w-8" />
              </Link>
              <Link to={routes.auth.sns.path}>
                <FaTwitter className="h-8 w-8" />
              </Link>
            </>
          )}
        </div>

        <ThemeBtn />
      </div>
    </nav>
  )
}
