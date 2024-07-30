package local.sunday00.ytbspringmongo.domain.score;

import jakarta.validation.Valid;
import local.sunday00.ytbspringmongo.domain.score.schema.Average;
import local.sunday00.ytbspringmongo.domain.score.schema.Score;
import local.sunday00.ytbspringmongo.domain.score.schema.AddScoreInput;
import lombok.extern.slf4j.Slf4j;
import net.datafaker.Faker;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.MutationMapping;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.security.access.annotation.Secured;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Controller;

import java.util.ArrayList;
import java.util.List;

@Slf4j
@Controller
public class ScoreController {
    @Secured("ROLE_ADMIN")
    @QueryMapping
    public List<Score> scores() {
        Faker faker = new Faker();

        List<Score> items = new ArrayList<>();

        for(int i=0; i<5; i++) {
            items.add(Score.builder()
                    .name(faker.name().fullName())
                            .score(faker.number().numberBetween(0, 100))
                    .build());
        }

        return items;
    }

    @Secured("ROLE_USER")
    @QueryMapping
    public Score myScore() {
        Faker faker = new Faker();

        return Score.builder()
                .name(faker.name().fullName())
                .score(faker.number().numberBetween(0, 100))
                .build();
    }

    @PreAuthorize("hasRole('admin')")
    @QueryMapping
    public Average average() {
        Faker faker = new Faker();

        return Average.builder()
                .count(faker.number().numberBetween(0, 6))
                .score(faker.number().numberBetween(0, 100))
                .build();
    }

    @MutationMapping
    public Boolean addScore(@Argument @Valid AddScoreInput addScoreInput) {
        return true;
    }
}
