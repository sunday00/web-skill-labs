import { defineConfig, loadEnv } from 'vite'
import react from '@vitejs/plugin-react-swc'
import tsconfigPaths from 'vite-tsconfig-paths'
import tailwindcss from '@tailwindcss/vite'

export default ({ mode }: { mode: string }) => {
  const env = loadEnv(mode, process.cwd(), 'VITE_APP_')

  console.log(env)

  return defineConfig({
    plugins: [react(), tsconfigPaths(), tailwindcss()],
    server: {
      port: Number(env.VITE_APP_PORT ?? 3000),
      host: '0.0.0.0',
    },
    build: {
      outDir: 'out/dist',
    },
  })
}
