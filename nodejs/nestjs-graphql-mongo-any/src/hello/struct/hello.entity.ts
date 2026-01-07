import { HydratedDocument } from 'mongoose'
import { Prop, Schema, SchemaFactory } from '@nestjs/mongoose'
import { Field, ObjectType } from '@nestjs/graphql'

export type HelloDocument = HydratedDocument<Hello>

@ObjectType()
@Schema({ collection: 'hello' })
export class Hello {
  @Field()
  id: string

  @Field()
  @Prop({ type: String })
  value: string
}

export const HelloSchema = SchemaFactory.createForClass(Hello)
