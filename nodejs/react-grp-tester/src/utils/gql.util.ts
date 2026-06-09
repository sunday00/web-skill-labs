export const gqlCommonContext = () => {
  return {
    headers: {
      // authorization: `bearer ${window.localStorage.getItem('accessToken')}`,
      'Content-Type': 'application/json',
      // 'x-dev-user-id': '6a0ed4167af6a42cf365c3eb',
      'x-dev-user-id': '6a0ec1ec0b591c417be58c40',
    },
  }
}
