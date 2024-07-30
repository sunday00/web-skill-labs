package local.sunday00.ytbspringmongo.domain.score.schema;

import lombok.Builder;

@Builder
public record Average(
        Integer count,
        Integer score
) {
}
