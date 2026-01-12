import { HydratedDocument } from 'mongoose'
import { Prop, Schema, SchemaFactory } from '@nestjs/mongoose'
import { Field, ObjectType } from '@nestjs/graphql'

export type FruitDocument = HydratedDocument<Fruit>

@ObjectType()
@Schema({ collection: 'fruits' })
export class Fruit {
  @Field()
  id: string

  @Field()
  @Prop({ type: String })
  name: string

  @Field()
  @Prop({ type: Date })
  lastOrder: Date
}

export const FruitSchema = SchemaFactory.createForClass(Fruit)
