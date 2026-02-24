-record(state, {
    mapping :: gleam@dict:dict(binary(), integer()),
    used_digits :: gleam@set:set(integer())
}).
