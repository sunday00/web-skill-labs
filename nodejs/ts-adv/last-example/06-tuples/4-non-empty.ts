type NonEmpty<T> = [T, ...T[]]

function submitTicket(subjects: NonEmpty<string>) {}

submitTicket(['custom ing...'])
submitTicket(['another', 'last'])
// submitTicket([]) // ERR
