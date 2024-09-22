package local.sunday00.ytbspringmongo.domain.fruits.schema;

import lombok.Builder;

@Builder
public record Producer(
        int id,
        String name
) {
}
