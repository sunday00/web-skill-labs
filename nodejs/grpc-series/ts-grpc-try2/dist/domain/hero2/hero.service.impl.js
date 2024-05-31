"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.heroService2 = exports.heroProto2 = void 0;
var protoLoader = require("@grpc/proto-loader");
var grpc = require("@grpc/grpc-js");
var packageDefinition = protoLoader.loadSync('./resources/grpc/domain/hero2/hero.proto');
exports.heroProto2 = grpc.loadPackageDefinition(packageDefinition);
exports.heroService2 = {
    FindMany: function (call) { },
    FindOne: function (call, callback) {
        console.log('hey2');
        callback(null, { id: 1, name: 'batman' });
    },
};
//# sourceMappingURL=hero.service.impl.js.map