import { GamesService } from './games.service';
export declare class GamesResolver {
    private readonly gamesService;
    constructor(gamesService: GamesService);
    getGames(): Promise<(import("mongoose").Document<unknown, {}, import("./schemas/game.schemas").GameEntity, {}, import("mongoose").DefaultSchemaOptions> & import("./schemas/game.schemas").GameEntity & {
        _id: import("mongoose").Types.ObjectId;
    } & {
        __v: number;
    })[]>;
}
