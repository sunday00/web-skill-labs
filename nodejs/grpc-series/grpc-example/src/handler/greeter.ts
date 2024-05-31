import * as grpc from '@grpc/grpc-js'

import { HelloRequest, HelloResponse } from '../proto/greeter/greeter_pb'
import { GreeterService, IGreeterServer } from '../proto/greeter/greeter_grpc_pb'

class GreeterHandler implements IGreeterServer {
  [key: string]: any;

  sayHello (call: grpc.ServerUnaryCall<HelloRequest, HelloResponse>, callback: grpc.sendUnaryData<HelloResponse>): void {
    const reply: HelloResponse = new HelloResponse();

    reply.setMessage(`Hello, ${ call.request.getName() }`);

    callback(null, reply);
  }
}

export default {
  service: GreeterService,                // Service interface
  handler: new GreeterHandler(),          // Service interface definitions
};