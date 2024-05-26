"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.heroService = void 0;
exports.heroService = {
    FindMany: function (call) { },
    FindOne: function (call, callback) {
        console.log('hey');
        callback(null, { id: 1, name: 'batman' });
    },
};
//# sourceMappingURL=hero.service.impl.js.map