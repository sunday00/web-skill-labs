package local.sunday00.ytbspringmongo.domain.fruits;

import graphql.GraphQLContext;
import graphql.schema.DataFetchingEnvironment;
import local.sunday00.ytbspringmongo.domain.fruits.schema.Fruit;
import lombok.extern.slf4j.Slf4j;
import net.datafaker.Faker;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.BatchMapping;
import org.springframework.graphql.data.method.annotation.QueryMapping;
//import org.springframework.graphql.data.method.annotation.SchemaMapping;
import org.springframework.stereotype.Controller;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Controller
public class FruitController {
    @QueryMapping(name = "fruit")
    public Fruit getFruit(@Argument String name, @Argument() String color, GraphQLContext context, DataFetchingEnvironment e) {

//        context.stream().forEach(m -> log.info("{} : {}",m.getKey(), m.getValue() ));
        log.info("name: {}, color: {}", name, color);

        return Fruit.builder()
                .name("Apple")
                .color("Red")
                .price(100)
                .build();
    }

    @QueryMapping
    public List<Fruit> fruits (DataFetchingEnvironment e) {
        Faker faker = new Faker();

        List<Fruit> fruits = new ArrayList<>();

        for(int i=0; i<5; i++) {
            fruits.add(Fruit.builder()
                            .name(faker.food().fruit())
                            .price(faker.number().numberBetween(1000, 10000))
                            .color(faker.color().name())
                    .build());
        }

         return fruits;
    }

//    @SchemaMapping
//    public BigDecimal sugar (Fruit fruit, GraphQLContext context, DataFetchingEnvironment e ) {
//
//        log.info("fruit: {}", fruit.name());
//
//        return BigDecimal.ONE;
//    }

//    @BatchMapping()
    @BatchMapping(field = "sugar", typeName = "Fruit")
    public Map<Fruit, BigDecimal> sugar(List<Fruit> fruits) {
        log.info("fruit: {}", fruits.stream().map(Fruit::name));

        Faker faker = new Faker();

        Map<Fruit, BigDecimal> sugarFruits = new HashMap<>();
        for(Fruit fruit : fruits) {
            sugarFruits.put(fruit, new BigDecimal(faker.number().digit()));
        }

        return sugarFruits;
    }

    // another way
    // https://techdozo.dev/spring-for-graphql-how-to-solve-the-n1-problem/
}
