package local.sunday00.ytbspringmongo.domain.hero.schema;

import local.sunday00.ytbspringmongo.connections.BaseEdge;
import local.sunday00.ytbspringmongo.connections.BasePageInfo;

import java.util.List;

public record HeroConnection(
        List<BaseEdge<Hero>> edges,
        BasePageInfo pageInfo
) {}

