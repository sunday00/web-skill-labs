import { Link } from '@remix-run/react'
import { NaverSnsIcon } from '@/components/icons/naver.sns.icon'

export default function SnsNaver() {
  const redirectUrl = encodeURI(window.env.SNS_NV_RD ?? '')
  const clientId = window.env.SNS_NV_ID ?? ''

  const scope = 'openid'

  const href =
    'https://nid.naver.com/oauth2.0/authorize' +
    '?' +
    `scope=${scope}&` +
    `state=${'CUSTOM_VALUE'}&` +
    `redirect_uri=${redirectUrl}&` +
    `client_id=${clientId}&` +
    `response_type=code`

  return (
    <Link
      to={href}
      className={`border border-1 border-gray-400 flex justify-center items-center w-[40px] h-[40px] rounded-md`}
    >
      <NaverSnsIcon />
    </Link>
  )
}
