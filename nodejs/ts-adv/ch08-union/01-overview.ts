// BAD CASE

type Response = {
  status: string
  error?: Error
}

let state: Response = {
  status: 'success',
  error: new Error('Something went wrong'),
}

type Response2 =
  | {
      status: 'loading'
    }
  | { status: 'success'; data: unknown }
  | { status: 'error'; error: Error }

let state2: Response2 = {
  status: 'success',
  // error: new Error('Something went wrong'), // <- success has no error
  data: {}, // <- success has data
}

let state3: Response2 = {
  status: 'error',
  error: new Error('Something went wrong'),
}

function handleState(state: Response2) {
  if (state.status === 'success') {
    return '1'
  }

  if (state.status === 'error') {
    return '2'
  }

  if (state.status === 'loading') {
    return '3'
  }

  // return state.status // impossible
}
