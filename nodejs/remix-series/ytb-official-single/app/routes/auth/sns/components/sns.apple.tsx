import { Link } from '@remix-run/react'
import { AppleSnsIcon } from '@/components/icons/apple.sns.icon'

export default function SnsApple() {
  const redirectUrl = encodeURI(window.env.SNS_AP_RD ?? '')
  const clientId = window.env.SNS_AP_ID ?? ''

  // const scope = 'email name'
  const scope = ''

  const href =
    'https://appleid.apple.com/auth/authorize' +
    '?' +
    `scope=${scope}&` +
    `state=${'CUSTOM_VALUE'}&` +
    `redirect_uri=${redirectUrl}&` +
    `client_id=${clientId}&` +
    `response_type=code` +
    '&response_mode=query'

  return (
    <Link
      to={href}
      className={`border border-1 border-gray-400 flex justify-center items-center w-[40px] h-[40px] rounded-md`}
    >
      <AppleSnsIcon />
    </Link>
  )
}
