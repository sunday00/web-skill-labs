interface Post {
  title: string
  body: string
  date: Date
}

interface FetchService<T> {
  fetchPosts(): T
}

let service: FetchService<string> = {
  fetchPosts: () => {
    return 'Fetched data'
  },
}

const data = service.fetchPosts()

// if (typeof data === "string") {
console.log(data.toLowerCase())
// } else if (isPost(data)) {
// }

function isPost(data: any): data is Post {
  return (<Post>data).body !== undefined
}
