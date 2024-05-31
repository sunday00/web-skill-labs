// GENERATED CODE -- DO NOT EDIT!

'use strict';
var grpc = require('@grpc/grpc-js');
var blog_pb = require('./blog_pb.js');
var google_protobuf_empty_pb = require('google-protobuf/google/protobuf/empty_pb.js');

function serialize_blog_BlogData(arg) {
  if (!(arg instanceof blog_pb.BlogData)) {
    throw new Error('Expected argument of type blog.BlogData');
  }
  return Buffer.from(arg.serializeBinary());
}

function deserialize_blog_BlogData(buffer_arg) {
  return blog_pb.BlogData.deserializeBinary(new Uint8Array(buffer_arg));
}

function serialize_blog_BlogId(arg) {
  if (!(arg instanceof blog_pb.BlogId)) {
    throw new Error('Expected argument of type blog.BlogId');
  }
  return Buffer.from(arg.serializeBinary());
}

function deserialize_blog_BlogId(buffer_arg) {
  return blog_pb.BlogId.deserializeBinary(new Uint8Array(buffer_arg));
}

function serialize_google_protobuf_Empty(arg) {
  if (!(arg instanceof google_protobuf_empty_pb.Empty)) {
    throw new Error('Expected argument of type google.protobuf.Empty');
  }
  return Buffer.from(arg.serializeBinary());
}

function deserialize_google_protobuf_Empty(buffer_arg) {
  return google_protobuf_empty_pb.Empty.deserializeBinary(new Uint8Array(buffer_arg));
}


var BlogService = exports.BlogService = {
  //  rpc Blog (BlogRequest) returns (BlogResponse);
createBlog: {
    path: '/blog.Blog/CreateBlog',
    requestStream: false,
    responseStream: false,
    requestType: blog_pb.BlogData,
    responseType: blog_pb.BlogId,
    requestSerialize: serialize_blog_BlogData,
    requestDeserialize: deserialize_blog_BlogData,
    responseSerialize: serialize_blog_BlogId,
    responseDeserialize: deserialize_blog_BlogId,
  },
  readBlog: {
    path: '/blog.Blog/ReadBlog',
    requestStream: false,
    responseStream: false,
    requestType: blog_pb.BlogId,
    responseType: blog_pb.BlogData,
    requestSerialize: serialize_blog_BlogId,
    requestDeserialize: deserialize_blog_BlogId,
    responseSerialize: serialize_blog_BlogData,
    responseDeserialize: deserialize_blog_BlogData,
  },
  updateBlog: {
    path: '/blog.Blog/UpdateBlog',
    requestStream: false,
    responseStream: false,
    requestType: blog_pb.BlogData,
    responseType: google_protobuf_empty_pb.Empty,
    requestSerialize: serialize_blog_BlogData,
    requestDeserialize: deserialize_blog_BlogData,
    responseSerialize: serialize_google_protobuf_Empty,
    responseDeserialize: deserialize_google_protobuf_Empty,
  },
  deleteBlog: {
    path: '/blog.Blog/DeleteBlog',
    requestStream: false,
    responseStream: false,
    requestType: blog_pb.BlogId,
    responseType: google_protobuf_empty_pb.Empty,
    requestSerialize: serialize_blog_BlogId,
    requestDeserialize: deserialize_blog_BlogId,
    responseSerialize: serialize_google_protobuf_Empty,
    responseDeserialize: deserialize_google_protobuf_Empty,
  },
  listBlogs: {
    path: '/blog.Blog/ListBlogs',
    requestStream: false,
    responseStream: true,
    requestType: google_protobuf_empty_pb.Empty,
    responseType: blog_pb.BlogData,
    requestSerialize: serialize_google_protobuf_Empty,
    requestDeserialize: deserialize_google_protobuf_Empty,
    responseSerialize: serialize_blog_BlogData,
    responseDeserialize: deserialize_blog_BlogData,
  },
};

exports.BlogClient = grpc.makeGenericClientConstructor(BlogService);
