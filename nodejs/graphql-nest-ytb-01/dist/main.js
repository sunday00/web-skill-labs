"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const core_1 = require("@nestjs/core");
const app_module_1 = require("./app.module");
async function bootstrap() {
    const configs = {
        port: 3000,
    };
    const app = await core_1.NestFactory.create(app_module_1.AppModule);
    await app.listen(configs.port);
    return configs;
}
bootstrap()
    .then((con) => {
    console.log(con.port);
})
    .catch((err) => {
    console.error(err);
});
//# sourceMappingURL=main.js.map