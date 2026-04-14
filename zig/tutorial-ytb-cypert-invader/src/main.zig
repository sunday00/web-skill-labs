const rl = @import("raylib");

const Rectangle = struct {
    x: f32,
    y: f32,
    width: f32,
    height: f32,

    pub inline fn intersect(self: Rectangle, other: Rectangle) bool {
        return self.x < other.x + other.width and
            self.x + self.width > other.x and
            self.y < other.y + other.height and
            self.y + self.height > other.y;
    }

    pub inline fn getRect(self: anytype) Rectangle {
        return .{
            .x = self.position_x,
            .y = self.position_y,
            .width = self.width,
            .height = self.height,
        };
    }
};

const GameConfig = struct {
    screenWidth: i32,
    screenHeight: i32,
    playerWidth: f32,
    playerHeight: f32,
    playerStartY: f32,
    bulletWidth: f32,
    bulletHeight: f32,
    shieldStartX: f32,
    shieldY: f32,
    shieldWidth: f32,
    shieldHeight: f32,
    shieldSpacing: f32,
    invaderStartX: f32,
    invaderStartY: f32,
    invaderWidth: f32,
    invaderHeight: f32,
    invaderSpacingX: f32,
    invaderSpacingY: f32,
};

const Player = struct {
    position_x: f32,
    position_y: f32,
    width: f32,
    height: f32,
    speed: f32,

    pub inline fn init(position_x: f32, position_y: f32, width: f32, height: f32) @This() {
        return .{
            .position_x = position_x,
            .position_y = position_y,
            .width = width,
            .height = height,
            .speed = 5.0,
        };
    }

    pub fn update(self: *@This()) void {
        if (rl.isKeyDown(rl.KeyboardKey.right)) {
            self.position_x += self.speed;
        }

        if (self.position_x < 0) {
            self.position_x = 0;
        }

        if (rl.isKeyDown(rl.KeyboardKey.left)) {
            self.position_x -= self.speed;
        }

        if (self.position_x + self.width > @as(f32, @floatFromInt(rl.getScreenWidth()))) {
            self.position_x = @as(f32, @floatFromInt(rl.getScreenWidth())) - self.width;
        }
    }

    pub inline fn getRect(self: @This()) Rectangle {
        return Rectangle.getRect(self);
    }

    pub inline fn draw(self: @This()) void {
        rl.drawRectangle(
            @intFromFloat(self.position_x),
            @intFromFloat(self.position_y),
            @intFromFloat(self.width),
            @intFromFloat(self.height),
            rl.Color.blue,
        );
    }
};

const Bullet = struct {
    position_x: f32,
    position_y: f32,
    width: f32,
    height: f32,
    speed: f32,
    active: bool,

    pub inline fn init(position_x: f32, position_y: f32, width: f32, height: f32) @This() {
        return .{
            .position_x = position_x,
            .position_y = position_y,
            .width = width,
            .height = height,
            .speed = 10.0,
            .active = false,
        };
    }

    pub fn update(self: *@This()) void {
        if (self.active) {
            self.position_y -= self.speed;
        }

        if (self.position_y < 0) {
            self.active = false;
        }
    }

    pub inline fn getRect(self: @This()) Rectangle {
        return Rectangle.getRect(self);
    }

    pub inline fn draw(self: @This()) void {
        if (self.active) {
            rl.drawRectangle(
                @intFromFloat(self.position_x),
                @intFromFloat(self.position_y),
                @intFromFloat(self.width),
                @intFromFloat(self.height),
                rl.Color.red,
            );
        }
    }
};

const Invader = struct {
    position_x: f32,
    position_y: f32,
    width: f32,
    height: f32,
    speed: f32,
    alive: bool,

    pub inline fn init(position_x: f32, position_y: f32, width: f32, height: f32) @This() {
        return .{
            .position_x = position_x,
            .position_y = position_y,
            .width = width,
            .height = height,
            .speed = 5.0,
            .alive = true,
        };
    }

    pub inline fn draw(self: @This()) void {
        if (self.alive) {
            rl.drawRectangle(
                @intFromFloat(self.position_x),
                @intFromFloat(self.position_y),
                @intFromFloat(self.width),
                @intFromFloat(self.height),
                rl.Color.green,
            );
        }
    }

    pub inline fn getRect(self: @This()) Rectangle {
        return Rectangle.getRect(self);
    }

    pub fn update(self: *@This(), dx: f32, dy: f32) void {
        self.position_x += dx;
        self.position_y += dy;
    }
};

const EnemyBullet = struct {
    position_x: f32,
    position_y: f32,
    width: f32,
    height: f32,
    speed: f32,
    active: bool,

    pub inline fn init(position_x: f32, position_y: f32, width: f32, height: f32) @This() {
        return .{
            .position_x = position_x,
            .position_y = position_y,
            .width = width,
            .height = height,
            .speed = 5.0,
            .active = false,
        };
    }

    pub inline fn getRect(self: @This()) Rectangle {
        return Rectangle.getRect(self);
    }

    pub fn update(self: *@This(), screen_height: i32) void {
        if (self.active) {
            self.position_y += self.speed;
            if (self.position_y > @as(f32, @floatFromInt(screen_height))) {
                self.active = false;
            }
        }
    }

    pub inline fn draw(self: @This()) void {
        if (self.active) {
            rl.drawRectangle(
                @intFromFloat(self.position_x),
                @intFromFloat(self.position_y),
                @intFromFloat(self.width),
                @intFromFloat(self.height),
                rl.Color.yellow,
            );
        }
    }
};

const Shield = struct {
    position_x: f32,
    position_y: f32,
    width: f32,
    height: f32,
    health: i32,

    pub inline fn init(position_x: f32, position_y: f32, width: f32, height: f32) @This() {
        return .{
            .position_x = position_x,
            .position_y = position_y,
            .width = width,
            .height = height,
            .health = 10,
        };
    }

    pub inline fn getRect(self: @This()) Rectangle {
        return Rectangle.getRect(self);
    }

    pub inline fn draw(self: @This()) void {
        if (self.health > 0) {
            const alpha = @as(u8, @intCast(@min(255, self.health * 25)));
            rl.drawRectangle(
                @intFromFloat(self.position_x),
                @intFromFloat(self.position_y),
                @intFromFloat(self.width),
                @intFromFloat(self.height),
                rl.Color{ .r = 0, .g = 255, .b = 255, .a = alpha },
            );
        }
    }
};

fn resetGame(
    player: *Player,
    bullets: []Bullet,
    enemy_bullets: []EnemyBullet,
    shields: []Shield,
    invaders: anytype,
    invader_direction: *f32,
    score: *i32,
    config: GameConfig,
) void {
    score.* = 0;
    player.* = Player.init(
        @as(f32, @floatFromInt(config.screenWidth)) / 2 - config.playerWidth / 2,
        @as(f32, @floatFromInt(config.screenHeight)) - 60.0,
        config.playerWidth,
        config.playerHeight,
    );
    for (bullets) |*bullet| {
        bullet.active = false;
    }
    for (enemy_bullets) |*bullet| {
        bullet.active = false;
    }
    for (shields, 0..) |*shield, i| {
        const x = config.shieldStartX + @as(f32, @floatFromInt(i)) * config.shieldSpacing;
        shield.* = Shield.init(x, config.shieldY, config.shieldWidth, config.shieldHeight);
    }
    for (invaders, 0..) |*row, i| {
        for (row, 0..) |*invader, j| {
            const x = config.invaderStartX + @as(f32, @floatFromInt(j)) * config.invaderSpacingX;
            const y = config.invaderStartY + @as(f32, @floatFromInt(i)) * config.invaderSpacingY;
            invader.* = Invader.init(x, y, config.invaderWidth, config.invaderHeight);
        }
    }

    invader_direction.* = 1.0;
}

pub fn main() !void {
    @setEvalBranchQuota(1100); // when reach default maximum inline
    rl.setTraceLogLevel(.none);

    const screenWidth = 800;
    const screenHeight = 600;

    const playerStartY = @as(f32, @floatFromInt(screenHeight)) - 60.0;
    const playerWidth = 50.0;
    const playerHeight = 30.0;

    const maxBullets = 10;
    const bulletWidth = 4.0;
    const bulletHeight = 10.0;

    var move_timer: i32 = 0;
    const invaderRows = 5;
    const invaderCols = 11;
    const invaderWidth = 40.0;
    const invaderHeight = 30.0;
    const invaderStartX = 100.0;
    const invaderStartY = 50.0;
    const invaderSpacingX = 60.0;
    const invaderSpacingY = 40.0;
    const invaderSpeed = 5.0;
    const invaderMoveDelay = 30;
    var invaderDirection: f32 = 1.0;
    const invaderDropDistance = 20.0;

    var enemy_shoot_timer: i32 = 0;
    const maxEnemyBullets = 20;
    const enemyShootDelay = 60;
    const enemyShootChance = 5;

    const shieldCount = 4;
    const shieldWidth = 80.0;
    const shieldHeight = 60.0;
    const shieldStartX = 150.0;
    const shieldY = 450.0;
    const shieldSpacing = 150.0;

    var score: i32 = 0;
    var victory = false;
    var gameover = false;

    const config = GameConfig{
        .playerStartY = playerStartY,
        .playerWidth = 50.0,
        .playerHeight = 30.0,

        .screenWidth = 800,
        .screenHeight = 600,

        .bulletWidth = 4.0,
        .bulletHeight = 10.0,

        .invaderWidth = 40.0,
        .invaderHeight = 30.0,
        .invaderStartX = 100.0,
        .invaderStartY = 50.0,
        .invaderSpacingX = 60.0,
        .invaderSpacingY = 40.0,

        .shieldWidth = 80.0,
        .shieldHeight = 60.0,
        .shieldY = 450.0,
        .shieldStartX = 150.0,
        .shieldSpacing = 150.0,
    };

    rl.initWindow(screenWidth, screenHeight, "Zig Invaders");
    defer rl.closeWindow();

    var player = Player.init(
        @as(f32, @floatFromInt(screenWidth)) / 2 - playerWidth / 2,
        playerStartY,
        playerWidth,
        playerHeight,
    );

    var bullets: [maxBullets]Bullet = undefined;
    inline for (&bullets) |*bullet| {
        bullet.* = Bullet.init(0, 0, bulletWidth, bulletHeight);
    }

    var enemy_bullets: [maxEnemyBullets]EnemyBullet = undefined;
    inline for (&enemy_bullets) |*bullet| {
        bullet.* = EnemyBullet.init(0, 0, bulletWidth, bulletHeight);
    }

    var invaders: [invaderRows][invaderCols]Invader = undefined;
    inline for (&invaders, 0..) |*row, i| {
        for (row, 0..) |*invader, j| {
            const x = invaderStartX + @as(f32, @floatFromInt(j)) * invaderSpacingX;
            const y = invaderStartY + @as(f32, @floatFromInt(i)) * invaderSpacingY;
            invader.* = Invader.init(x, y, invaderWidth, invaderHeight);
        }
    }

    var shields: [shieldCount]Shield = undefined;
    inline for (&shields, 0..) |*shield, i| {
        const x = shieldStartX + @as(f32, @floatFromInt(i)) * shieldSpacing;
        shield.* = Shield.init(x, shieldY, shieldWidth, shieldHeight);
    }

    rl.setTargetFPS(60);

    while (!rl.windowShouldClose()) {
        rl.beginDrawing();
        defer rl.endDrawing();

        rl.clearBackground(rl.Color.black);

        if (gameover) {
            rl.drawText("GameOver", 270, 250, 40, rl.Color.red);
            const score_text = rl.textFormat("Final Score %d", .{score});
            rl.drawText(score_text, 285, 310, 30, rl.Color.white);
            rl.drawText("Press ENTER to play again or ESC to quit", 180, 360, 20, rl.Color.green);

            if (rl.isKeyPressed(rl.KeyboardKey.enter)) {
                gameover = false;
                resetGame(
                    &player,
                    &bullets,
                    &enemy_bullets,
                    &shields,
                    &invaders,
                    &invaderDirection,
                    &score,
                    config,
                );
            }

            continue;
        }

        if (victory) {
            rl.drawText("YOU WIN!", 320, 250, 40, rl.Color.gold);
            const score_text = rl.textFormat("Final Score %d", .{score});
            rl.drawText(score_text, 280, 310, 30, rl.Color.white);
            rl.drawText("Press ENTER to play again or ESC to quit", 180, 360, 20, rl.Color.green);

            if (rl.isKeyPressed(rl.KeyboardKey.enter)) {
                victory = false;
                resetGame(
                    &player,
                    &bullets,
                    &enemy_bullets,
                    &shields,
                    &invaders,
                    &invaderDirection,
                    &score,
                    config,
                );
            }
            continue;
        }

        // update ==============================

        player.update();

        if (rl.isKeyPressed(rl.KeyboardKey.space)) {
            inline for (&bullets) |*bullet| {
                if (!bullet.active) {
                    bullet.position_x = player.position_x + player.width / 2 - bullet.width / 2;
                    bullet.position_y = player.position_y;
                    bullet.active = true;
                    break;
                }
            }
        }

        for (&bullets) |*bullet| {
            bullet.update();
        }

        for (&bullets) |*bullet| {
            if (bullet.active) {
                inline for (&invaders) |*row| {
                    for (row) |*invader| {
                        if (invader.alive) {
                            if (bullet.getRect().intersect(invader.getRect())) {
                                bullet.active = false;
                                invader.alive = false;

                                score += 10;
                                break;
                            }
                        }
                    }
                }

                inline for (&shields) |*shield| {
                    if (shield.health > 0) {
                        if (bullet.getRect().intersect(shield.getRect())) {
                            bullet.active = false;
                            shield.health -= 1;
                            break;
                        }
                    }
                }
            }
        }

        inline for (&enemy_bullets) |*bullet| {
            bullet.update(screenHeight);

            if (bullet.active) {
                if (bullet.getRect().intersect(player.getRect())) {
                    bullet.active = false;
                    gameover = true;
                }

                for (&shields) |*shield| {
                    if (shield.health > 0) {
                        if (bullet.getRect().intersect(shield.getRect())) {
                            bullet.active = false;
                            shield.health -= 1;
                            break;
                        }
                    }
                }
            }
        }

        enemy_shoot_timer += 1;
        if (enemy_shoot_timer >= enemyShootDelay) {
            enemy_shoot_timer = 0;
            inline for (&invaders) |*row| {
                for (row) |*invader| {
                    if (invader.alive and rl.getRandomValue(0, 100) < enemyShootChance) {
                        inline for (&enemy_bullets) |*bullet| {
                            if (!bullet.active) {
                                bullet.position_x = invader.position_x + invader.width / 2 - bullet.width / 2;
                                bullet.position_y = invader.position_y + invaderHeight;
                                bullet.active = true;
                                break;
                            }
                        }
                        break;
                    }
                }
            }
        }

        move_timer += 1;
        if (move_timer >= invaderMoveDelay) {
            move_timer = 0;

            var hit_edge = false;
            inline for (&invaders) |*row| {
                for (row) |*invader| {
                    if (invader.alive) {
                        const next_x = invader.position_x + (invaderSpeed * invaderDirection);
                        if (next_x < 0 or next_x + invader.width > @as(f32, @floatFromInt(screenWidth))) {
                            hit_edge = true;
                            break;
                        }
                    }
                }

                if (hit_edge) break;
            }

            if (hit_edge) {
                invaderDirection *= -1.0;
                inline for (&invaders) |*row| {
                    for (row) |*invader| {
                        invader.update(0, invaderDropDistance);
                    }
                }
            } else {
                inline for (&invaders) |*row| {
                    for (row) |*invader| {
                        invader.update(invaderSpeed * invaderDirection, 0);
                    }
                }
            }

            inline for (&invaders) |*row| {
                for (row) |*invader| {
                    if (invader.alive) {
                        if (invader.getRect().intersect(player.getRect())) {
                            invader.alive = false;
                            gameover = true;
                        }

                        if (invader.position_y + (invader.height / 2) >= screenHeight) gameover = true;
                    }
                }
            }
        }

        var allDead = true;
        outerLoop: inline for (&invaders) |*row| {
            for (row) |*invader| {
                if (invader.alive) {
                    allDead = false;
                    break :outerLoop;
                }
            }
        }
        if (allDead) victory = true;

        // draw ==============================

        inline for (&shields) |*shield| {
            shield.draw();
        }

        player.draw();

        inline for (&bullets) |*bullet| {
            bullet.draw();
        }

        inline for (&invaders) |*row| {
            for (row) |*invader| {
                invader.draw();
            }
        }

        inline for (&enemy_bullets) |*bullet| {
            bullet.draw();
        }

        const score_text = rl.textFormat("Score: %d", .{score});
        rl.drawText(score_text, 20, screenHeight - 20, 20, rl.Color.white);
        // rl.drawText("Zig Invaders", 300, 250, 40, rl.Color.green);
        rl.drawText("Press space to shoot, Esc to quit", 20, 20, 20, rl.Color.green);
    }
}
