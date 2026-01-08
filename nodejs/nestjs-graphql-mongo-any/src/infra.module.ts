import { Global, Module } from '@nestjs/common'
import { ConfigModule, ConfigService } from '@nestjs/config'
import { JwtModule } from '@nestjs/jwt'
import { CqrsModule } from '@nestjs/cqrs'
import env from './_common/config/env'
import { jwt, jwtOptions } from './_common/config/jwt.config'
import { dbConn } from './_common/config/db.config'
import { MongooseModule } from '@nestjs/mongoose'
import { GraphQLModule } from '@nestjs/graphql'
import { ApolloDriver, ApolloDriverConfig } from '@nestjs/apollo'
import { ApolloServerPluginLandingPageLocalDefault } from '@apollo/server/plugin/landingPage/default'
import { GraphQLError } from 'graphql/error'
import { FirebaseModule } from './_common/modules/firebase/firebase.module'

@Global()
@Module({
  imports: [
    ConfigModule.forRoot({ isGlobal: true, load: [env, jwt, dbConn] }),
    FirebaseModule,
    CqrsModule.forRoot(),
    MongooseModule.forRootAsync({
      imports: [ConfigModule],
      useFactory: (configService: ConfigService) => {
        console.log(
          '🛢️ dbConfig.connString',
          configService.get<string>('dbConfig.connString'),
        )

        return {
          uri: configService.get<string>('dbConfig.connString'),
          dbName: configService.get<string>('dbConfig.dbName'),
        }
      },
      inject: [ConfigService],
    }),
    JwtModule.register(jwtOptions()),
    GraphQLModule.forRoot<ApolloDriverConfig>({
      driver: ApolloDriver,
      playground: false,
      autoSchemaFile: './src/schema.gql',
      plugins: [ApolloServerPluginLandingPageLocalDefault()],
      context: ({ req, res }) => ({ req, res }),
      formatError: (error: GraphQLError) => {
        const { stacktrace: _st, ...extensions } = error.extensions

        return new GraphQLError(
          error.message ??
            (error?.extensions?.originalError as { message: string })
              ?.message ??
            'uncaughtException',
          {
            extensions: {
              ...extensions,
            },
          },
        )
      },
    }),
  ],
})
export class InfraModule {}
