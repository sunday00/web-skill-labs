"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.heroService = exports.heroProto = void 0;
var protoLoader = require("@grpc/proto-loader");
var grpc = require("@grpc/grpc-js");
var packageDefinition = protoLoader.loadSync('./resources/grpc/domain/hero/hero.proto');
exports.heroProto = grpc.loadPackageDefinition(packageDefinition);
exports.heroService = {
    FindMany: function (call) { },
    FindOne: function (call, callback) {
        console.log('hey');
        callback(null, { id: 1, name: 'batman' });
    },
};
//# sourceMappingURL=hero.service.impl.js.map