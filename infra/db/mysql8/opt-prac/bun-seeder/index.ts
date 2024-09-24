import { services } from "./src/services";
import { RunOption } from "./src/share/types";
import { MysqlClient } from "./src/utils/mysql";

const main = async () => {
  const [envName, initFile, ...args] = process.argv;

  const options: RunOption = {};
  for (const op of args) {
    const [k, v] = op.split("=");
    options[k] = isNaN(Number(v)) ? v : Number(v);
  }

  if (!options.service) throw new Error("not specify service");

  const db = new MysqlClient();
  await db.connect();

  const Service = services[options.service];
  const service = new Service();
  service.connect(db);

  try {
    // @ts-ignore
    await service[options.task ?? "createMany"](options);

    console.log("fin");

    return 0;
  } catch (e) {
    console.error(e);
    return 1;
  }
};

main().then((r) => {
  if (!r) process.exit();
  else process.abort();
});
