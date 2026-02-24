import { HydratedDocument } from 'mongoose';
export type GameDocument = HydratedDocument<GameEntity>;
export declare class GameEntity {
    name: string;
    genre: string;
}
export declare const GameSchema: import("mongoose").Schema<GameEntity, import("mongoose").Model<GameEntity, any, any, any, import("mongoose").Document<unknown, any, GameEntity, any, import("mongoose").DefaultSchemaOptions> & GameEntity & {
    _id: import("mongoose").Types.ObjectId;
} & {
    __v: number;
}, any, GameEntity>, {}, {}, {}, {}, import("mongoose").DefaultSchemaOptions, GameEntity, import("mongoose").Document<unknown, {}, GameEntity, {
    id: string;
}, import("mongoose").ResolveSchemaOptions<import("mongoose").DefaultSchemaOptions>> & Omit<GameEntity & {
    _id: import("mongoose").Types.ObjectId;
} & {
    __v: number;
}, "id"> & {
    id: string;
}, {
    name?: import("mongoose").SchemaDefinitionProperty<string, GameEntity, import("mongoose").Document<unknown, {}, GameEntity, {
        id: string;
    }, import("mongoose").ResolveSchemaOptions<import("mongoose").DefaultSchemaOptions>> & Omit<GameEntity & {
        _id: import("mongoose").Types.ObjectId;
    } & {
        __v: number;
    }, "id"> & {
        id: string;
    }>;
    genre?: import("mongoose").SchemaDefinitionProperty<string, GameEntity, import("mongoose").Document<unknown, {}, GameEntity, {
        id: string;
    }, import("mongoose").ResolveSchemaOptions<import("mongoose").DefaultSchemaOptions>> & Omit<GameEntity & {
        _id: import("mongoose").Types.ObjectId;
    } & {
        __v: number;
    }, "id"> & {
        id: string;
    }>;
}, GameEntity>;
