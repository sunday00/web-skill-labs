import { generateCookie, getCookie } from '@/routes/auth/signin/cookie.manager'
import { CommonRes, ManualError } from '@/common/common.entity'
import { time } from '@/utils/time'

export enum METHOD {
  GET = 'GET',
  POST = 'POST',
  PUT = 'PUT',
  PATCH = 'PATCH',
  DELETE = 'DELETE',
}

type Props = {
  request: Request
  method: METHOD
  url: string
  data: unknown
}

/**
 * @description
 * - fetch with credential accessToken auto.
 * - serialize querystring.
 * - auto refresh accessToken when res has refreshToken.
 * - returns json stringify string, so you should parse first.
 * @example usage: JSON.parse( useLoaderData<typeof loader>() )
 * @response
 * ```
 * Promise<
 *  Response<
 *    JSON.stringfy(CommonRes<T>),
 *    Headers({Cookie-Set: 'access-token'})
 *  >
 * >
 * ```

 * */
export const refreshableFetch = async <T>({ request, method, url, data }: Props) => {
  const accessToken = await getCookie('access-token', request)

  if (!accessToken) {
    return new Response(
      JSON.stringify(ManualError({ statusCode: 401, message: 'needToLogin' })),
      {},
    )
  }

  const hostedUrl =
    `${process.env.API_HOST}/api/v1${url}` +
    `${method === METHOD.GET || method === METHOD.DELETE ? `?${new URLSearchParams(data as URLSearchParams)}` : ''}`

  const options: {
    method: METHOD
    headers: { [k: string]: string }
    body?: string
  } = {
    method,
    headers: {
      Accept: 'application/json',
      Authorization: 'Bearer ' + accessToken,
    },
  }

  if (method !== METHOD.GET && method !== METHOD.DELETE) {
    options.headers['Content-Type'] = 'application/json'
    options.body = JSON.stringify(data)
  }

  const raw = await fetch(hostedUrl, options)
  const json: CommonRes<T> = await raw.json()

  if (
    'data' in json &&
    'refreshToken' in json.data &&
    (json.data.refreshToken as string)?.length > 0
  ) {
    // const resPayload = parseJwt(json.data.refreshToken!)

    const resHeaders = new Headers()
    resHeaders.append(
      'Set-Cookie',
      await generateCookie(
        'access-token',
        // resPayload.exp + 20 * 1000,
        time().add(1, 'weeks').unix() * 1000,
        json.data.refreshToken!,
      ),
    )

    return new Response(JSON.stringify(json), {
      headers: resHeaders,
    })
  }

  return new Response(JSON.stringify(json), { headers: {} as Headers })
}
