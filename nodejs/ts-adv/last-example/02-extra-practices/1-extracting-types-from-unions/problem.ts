export type Action =
  | {
      method: 'GET'
      description: 'Fetch users.'
    }
  | {
      method: 'POST'
      description: 'Add a user.'
    }
  | {
      method: 'DELETE'
      description: 'Delete a user.'
    }

// type RequestActions = any;
// type RequestActions = Extract<Action, { method: 'DELETE' }>

type Methods<T> = T extends { method: infer U } ? U : never

type RequestActions<U extends Methods<Action>> = Extract<Action, { method: U }>

type DeleteAction = RequestActions<'DELETE'>

export {}
