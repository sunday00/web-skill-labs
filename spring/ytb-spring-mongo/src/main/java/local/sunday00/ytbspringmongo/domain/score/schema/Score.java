package local.sunday00.ytbspringmongo.domain.score.schema;

import lombok.Builder;

@Builder
public record Score (
        String name,
        Integer score
) {}
