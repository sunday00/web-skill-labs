const fs = require('fs')
const grpc = require('@grpc/grpc-js')
const { BlogClient } = require('../blog/proto/blog_grpc_pb')
const { BlogData, BlogId } = require('../blog/proto/blog_pb')

const createBlog = async (client) => {
  console.log('====---- Creating blog method invoked ----====')

  return await new Promise((resolve, reject) => {
    const req = new BlogData()
      .setAuthorId('Clement')
      .setTitle(`this is article witten at ${Date.now()}`)
      .setContent(`the lorem witten at ${Date.now()} ... wow...`)

    client.createBlog(req, (err, res) => {
      if (err) reject(err)

      console.log(`Created. ${res}`)
      resolve(res.getId())
    })
  })
}

const readBlog = async (client, id) => {
  console.log('====---- Read blog method invoked ----====')

  return new Promise((resolve, reject) => {
    const req = new BlogId().setId(id)
    client.readBlog(req, (err, res) => {
      if (err) reject(err)

      console.log(`Read blog ${res}`)
      resolve()
    })
  })
}

const updateBlog = async (client, id) => {
  console.log('====---- Update blog method invoked ----====')

  return new Promise((resolve, reject) => {
    const req = new BlogData()
      .setId(id)
      .setAuthorId('Clement2')
      .setTitle(`this is article witten at ${Date.now()} updated`)
      .setContent(`the lorem witten at ${Date.now()} on update content`)

    client.updateBlog(req, (err, res) => {
      if (err) reject(err)

      console.log(`Updated blog id: ${res}`)
      resolve()
    })
  })
}

const main = async () => {
  let creds = grpc.ChannelCredentials.createInsecure()

  const client = new BlogClient('127.0.0.1:50051', creds)

  // const id = await createBlog(client)
  // console.log({ id })

  // await readBlog(client, id)
  // await readBlog(client, '6649f919bb392b54a04193ff')

  await updateBlog(client, '6649f919bb392b54a04193fe')

  client.close()
}

main()
