package local.sunday00.ytbspringmongo.domain.fruits.schema;

import lombok.Builder;

import java.math.BigDecimal;

@Builder
public record Fruit(
        String name,
        String color,
        Integer price,
        BigDecimal sugar
) {}
