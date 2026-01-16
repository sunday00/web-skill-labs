import { defineConfig, loadEnv } from 'vite'
import tsconfigPaths from 'vite-tsconfig-paths'

export default ({ mode }: { mode: string }) => {
  const env = loadEnv(mode, process.cwd(), 'VITE_APP_')

  console.log(env)

  return defineConfig({
    plugins: [tsconfigPaths()],
    server: {
      port: Number(env.VITE_APP_PORT ?? 3000),
      host: '0.0.0.0',
    },
    build: {
      outDir: 'out/dist',
    },
  })
}
