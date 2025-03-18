import { Link } from '@remix-run/react'
import { FaComment } from 'react-icons/fa6'

export default function SnsKakao() {
  const redirectUrl = encodeURI(window.env.SNS_KK_RD ?? '')
  const clientId = window.env.SNS_KK_ID ?? ''

  const scope = 'openid'

  const href =
    'https://kauth.kakao.com/oauth/authorize' +
    '?' +
    `scope=${scope}&` +
    `state=${'CUSTOM_VALUE'}&` +
    `redirect_uri=${redirectUrl}&` +
    `client_id=${clientId}&` +
    `response_type=code`

  return (
    <Link
      to={href}
      className={`border border-1 border-gray-400 flex justify-center items-center w-[40px] h-[40px] bg-[#ffd43b] rounded-md`}
    >
      <FaComment className={'text-[#2c2e33] text-xl'} />
    </Link>
  )
}
