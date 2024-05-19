const grpc = require('@grpc/grpc-js')
const pb = require('../blog/proto/blog_pb')
const { sleep } = require('../utils/time')
const { ObjectId } = require('mongodb')
const { Empty } = require('google-protobuf/google/protobuf/empty_pb')

const blogToDocument = (blog) => {
  return {
    author_id: blog.getAuthorId(),
    title: blog.getTitle(),
    content: blog.getContent(),
  }
}

const documentToBlog = (doc) => {
  return new pb.BlogData()
    .setId(doc._id.toString())
    .setAuthorId(doc.author_id)
    .setTitle(doc.title)
    .setContent(doc.content)
}

const internal = (err, callback) => {
  return callback({
    code: grpc.status.INTERNAL,
    message: err.toString(),
  })
}

const checkNotAcknowledged = (res, callback) => {
  if (!res.acknowledged) {
    callback({
      code: grpc.status.INTERNAL,
      message: `Operation wasn't acknowledged`,
    })
  }
}

const checkOid = (id, callback) => {
  try {
    return new ObjectId(id)
  } catch (err) {
    callback({
      code: grpc.status.INTERNAL,
      message: err.toString(),
    })
  }
}

const checkNotFound = (res, callback) => {
  if (!res || res.matchedCount === 0) {
    callback({
      code: grpc.status.NOT_FOUND,
      message: 'Not found',
    })
  }
}

exports.createBlog = async (call, callback) => {
  const data = blogToDocument(call.request)

  await collection
    .insertOne(data)
    .then((res) => {
      checkNotAcknowledged(res, callback)

      const id = res.insertedId.toString()
      const blogId = new pb.BlogId().setId(id)

      callback(null, blogId)
    })
    .catch((err) => internal(err, callback))
}

exports.readBlog = async (call, callback) => {
  const oid = checkOid(call.request.getId(), callback)

  await collection
    .findOne({ _id: oid })
    .then((res) => {
      checkNotFound(res, callback)

      callback(null, documentToBlog(res))
    })
    .catch((err) => internal(err, callback))
}

exports.updateBlog = async (call, callback) => {
  const oid = checkOid(call.request.getId(), callback)

  await collection
    .updateOne({ _id: oid }, { $set: blogToDocument(call.request) })
    .then((res) => {
      checkNotFound(res, callback)
      checkNotAcknowledged(res, callback)

      callback(null, new Empty())
    })
    .catch((err) => internal(err, callback))
}
