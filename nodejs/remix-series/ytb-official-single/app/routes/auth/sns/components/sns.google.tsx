import { Link } from '@remix-run/react'
import { GoogleSnsIcon } from '@/components/icons/google.sns.icon'

export default function SnsGoogle() {
  const redirectUrl = encodeURI(window.env.SNS_GG_RD ?? '')
  const clientId = encodeURI(window.env.SNS_GG_ID ?? '')

  const scope =
    'https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.email' +
    '+https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fuserinfo.profile' +
    '+openid'

  const href =
    'https://accounts.google.com/o/oauth2/v2/auth' +
    '?' +
    `scope=${scope}&` +
    'access_type=offline&include_granted_scopes=true&response_type=code&' +
    `state=${'CUSTOM_VALUE'}&` +
    `redirect_uri=${redirectUrl}&` +
    `client_id=${clientId}`

  return (
    <Link to={href}>
      <GoogleSnsIcon />
    </Link>
  )
}
