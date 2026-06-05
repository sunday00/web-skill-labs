export const gqlCommonContext = () => {
  return {
    headers: {
      authorization: `bearer ${window.localStorage.getItem('accessToken')}`,
      'Content-Type': 'application/json',
    },
  }
}
