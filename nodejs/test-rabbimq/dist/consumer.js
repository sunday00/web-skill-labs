"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const amqplib = require("amqplib");
const getRabbitMqConnection = () => __awaiter(void 0, void 0, void 0, function* () {
    return yield amqplib.connect({
        hostname: '127.0.0.1',
        port: 5672,
        username: 'sunday00',
        password: 'example',
    }, amqplib.credentials.external());
});
const boot = () => __awaiter(void 0, void 0, void 0, function* () {
    const conn = yield getRabbitMqConnection();
    const ch = yield conn.createChannel();
    yield ch.assertExchange('test.nodejs.delay', 'direct', { durable: true });
    yield ch.assertQueue('say.status', { durable: true });
    yield ch.bindQueue('say.status', 'test.nodejs.delay', 'task.fire');
    yield ch.consume('say.status', (payload) => {
        const raw = payload.content.toString('utf-8');
        const json = JSON.parse(raw);
        console.log(json);
    });
});
boot();
