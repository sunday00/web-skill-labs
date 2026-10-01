import path from 'node:path'
import { fileURLToPath } from 'node:url'
import rspack from '@rspack/core'

const __dirname = path.dirname(fileURLToPath(import.meta.url))

export default {
  mode: 'production',
  target: 'node20',
  entry: './src/main.ts',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'main.js',
    module: true,
    chunkFormat: 'module',
    library: { type: 'module' },
  },
  experiments: { outputModule: true },
  optimization: {
    minimize: true,
    minimizer: [
      new rspack.SwcJsMinimizerRspackPlugin({
        minimizerOptions: {
          mangle: {
            reserved: [
              'require',
              '__webpack_require__',
              '__filename',
              '__dirname',
              'parentPort',
              'workerData',
            ],
            keep_classnames: true,
            keep_fnames: true,
          },
        },
      }),
    ],
  },
  externals: [],
  externalsPresets: { node: true },
  resolve: {
    extensions: ['.ts', '.js', '.mjs'],
    extensionAlias: {
      '.js': ['.ts', '.js'],
    },
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        exclude: /node_modules/,
        loader: 'builtin:swc-loader',
        options: {
          jsc: {
            parser: { syntax: 'typescript', decorators: true },
            transform: {
              legacyDecorator: true,
              decoratorMetadata: true,
            },
            target: 'es2022',
          },
        },
      },
    ],
  },
  plugins: [
    new rspack.IgnorePlugin({
      resourceRegExp:
        /^(@nestjs\/microservices|@grpc\/grpc-js|@grpc\/proto-loader|amqp-connection-manager|amqplib|kafkajs|mqtt|nats|@fastify\/static|@fastify\/view)(\/.*)?$/,
    }),
    new rspack.BannerPlugin({
      banner: [
        `import { createRequire as __rspackCreateRequire } from 'node:module';`,
        `import { fileURLToPath as __rspackFileURLToPath } from 'node:url';`,
        `import { dirname as __rspackDirname } from 'node:path';`,
        `const __filename = __rspackFileURLToPath(import.meta.url);`,
        `const __dirname = __rspackDirname(__filename);`,
        `const require = __rspackCreateRequire(import.meta.url);`,
      ].join('\n'),
      raw: true,
      entryOnly: true,
    }),
  ],
}