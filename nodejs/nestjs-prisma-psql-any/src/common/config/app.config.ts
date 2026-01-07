import { registerAs } from '@nestjs/config';

export const appConfig = registerAs('appConfig', () => ({
  port: 3033,
}));

export const configModuleOption = {
  isGlobal: true,
  envFilePath: `.env.dev`,
  load: [appConfig],
};
