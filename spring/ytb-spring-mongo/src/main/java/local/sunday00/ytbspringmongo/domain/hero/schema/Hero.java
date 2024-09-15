package local.sunday00.ytbspringmongo.domain.hero.schema;

import lombok.Builder;

import java.util.Optional;

@Builder
public record Hero (
        String name,
        Optional<HeroSkill> ability,
        HeroSide mind,
        Optional<Hero> sideKick
) {}
