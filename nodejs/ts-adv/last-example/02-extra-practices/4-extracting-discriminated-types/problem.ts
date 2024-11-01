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

// type Methods<T> = T extends { method: infer U } ? U : never
// type ActionType = Methods<Action>

type ActionType = Action['method']

export {}
