// GENERATED CODE -- DO NOT EDIT!

'use strict';
var grpc = require('@grpc/grpc-js');
var simple_pb = require('./simple_pb.js');
var sum_pb = require('./sum_pb.js');
var prime_pb = require('./prime_pb.js');

function serialize_calculator_PrimeRequest(arg) {
  if (!(arg instanceof prime_pb.PrimeRequest)) {
    throw new Error('Expected argument of type calculator.PrimeRequest');
  }
  return Buffer.from(arg.serializeBinary());
}

function deserialize_calculator_PrimeRequest(buffer_arg) {
  return prime_pb.PrimeRequest.deserializeBinary(new Uint8Array(buffer_arg));
}

function serialize_calculator_PrimeResponse(arg) {
  if (!(arg instanceof prime_pb.PrimeResponse)) {
    throw new Error('Expected argument of type calculator.PrimeResponse');
  }
  return Buffer.from(arg.serializeBinary());
}

function deserialize_calculator_PrimeResponse(buffer_arg) {
  return prime_pb.PrimeResponse.deserializeBinary(new Uint8Array(buffer_arg));
}

function serialize_calculator_SimpleRequest(arg) {
  if (!(arg instanceof simple_pb.SimpleRequest)) {
    throw new Error('Expected argument of type calculator.SimpleRequest');
  }
  return Buffer.from(arg.serializeBinary());
}

function deserialize_calculator_SimpleRequest(buffer_arg) {
  return simple_pb.SimpleRequest.deserializeBinary(new Uint8Array(buffer_arg));
}

function serialize_calculator_SimpleResponse(arg) {
  if (!(arg instanceof simple_pb.SimpleResponse)) {
    throw new Error('Expected argument of type calculator.SimpleResponse');
  }
  return Buffer.from(arg.serializeBinary());
}

function deserialize_calculator_SimpleResponse(buffer_arg) {
  return simple_pb.SimpleResponse.deserializeBinary(new Uint8Array(buffer_arg));
}

function serialize_calculator_SumRequest(arg) {
  if (!(arg instanceof sum_pb.SumRequest)) {
    throw new Error('Expected argument of type calculator.SumRequest');
  }
  return Buffer.from(arg.serializeBinary());
}

function deserialize_calculator_SumRequest(buffer_arg) {
  return sum_pb.SumRequest.deserializeBinary(new Uint8Array(buffer_arg));
}

function serialize_calculator_SumResponse(arg) {
  if (!(arg instanceof sum_pb.SumResponse)) {
    throw new Error('Expected argument of type calculator.SumResponse');
  }
  return Buffer.from(arg.serializeBinary());
}

function deserialize_calculator_SumResponse(buffer_arg) {
  return sum_pb.SumResponse.deserializeBinary(new Uint8Array(buffer_arg));
}


var CalculatorService = exports.CalculatorService = {
  simple: {
    path: '/calculator.Calculator/Simple',
    requestStream: false,
    responseStream: true,
    requestType: simple_pb.SimpleRequest,
    responseType: simple_pb.SimpleResponse,
    requestSerialize: serialize_calculator_SimpleRequest,
    requestDeserialize: deserialize_calculator_SimpleRequest,
    responseSerialize: serialize_calculator_SimpleResponse,
    responseDeserialize: deserialize_calculator_SimpleResponse,
  },
  sum: {
    path: '/calculator.Calculator/Sum',
    requestStream: false,
    responseStream: false,
    requestType: sum_pb.SumRequest,
    responseType: sum_pb.SumResponse,
    requestSerialize: serialize_calculator_SumRequest,
    requestDeserialize: deserialize_calculator_SumRequest,
    responseSerialize: serialize_calculator_SumResponse,
    responseDeserialize: deserialize_calculator_SumResponse,
  },
  prime: {
    path: '/calculator.Calculator/Prime',
    requestStream: false,
    responseStream: true,
    requestType: prime_pb.PrimeRequest,
    responseType: prime_pb.PrimeResponse,
    requestSerialize: serialize_calculator_PrimeRequest,
    requestDeserialize: deserialize_calculator_PrimeRequest,
    responseSerialize: serialize_calculator_PrimeResponse,
    responseDeserialize: deserialize_calculator_PrimeResponse,
  },
};

exports.CalculatorClient = grpc.makeGenericClientConstructor(CalculatorService);
