import { GameEntity } from './schemas/game.schemas';
import { Model } from 'mongoose';
export declare class GamesService {
    private gameModel;
    constructor(gameModel: Model<GameEntity>);
    getGames(): Promise<(import("mongoose").Document<unknown, {}, GameEntity, {}, import("mongoose").DefaultSchemaOptions> & GameEntity & {
        _id: import("mongoose").Types.ObjectId;
    } & {
        __v: number;
    })[]>;
}
