const todos = [{message: "breakfast"}]

const actions = {
  getTodos: (payload) => todos,
  createTodo: (payload) => {
    const newTodo = {
      message: payload.message
    }

    todos.push(newTodo)
    return { success: true}
  },
}

module.exports = actions