package local.sunday00.ytbspringmongo.fruits;

import local.sunday00.ytbspringmongo.domain.fruits.FruitController;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.graphql.tester.AutoConfigureGraphQlTester;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.graphql.test.tester.GraphQlTester;

@SpringBootTest
@AutoConfigureGraphQlTester
class FruitControllerTest {
    @Autowired
    private FruitController fruitController;
    @Autowired
    private GraphQlTester graphQlTester;

    @Test
    void getOneFruitTest() {
        graphQlTester.documentName("fruit")
                .variable("name", "apple")
                .execute()
                .path("fruit.name")
                .entity(String.class)
                .isEqualTo("Apple");
    }
}
