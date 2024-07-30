package local.sunday00.ytbspringmongo.domain.fitness;

import graphql.GraphQLContext;
import graphql.GraphQLException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.UUID;

@Slf4j
@Controller
public class FitnessController {

    @QueryMapping
    public List<FitnessClass> schedule(@Argument Coach coach, GraphQLContext context) {
        log.info("schedule({})", coach);

        var startsAt = LocalDateTime.now();

        if(true){
            throw new GraphQLException("Enforced Error");
        }

        return List.of(
                FitnessClass.builder()
                        .id(UUID.randomUUID())
                        .coach(coach)
                        .startsAt(startsAt)
                        .endsAt(startsAt.plusHours(1L))
                        .customDate(startsAt)
                        .difficulty(Difficulty.BEGINNER)
                        .build()
        );
    }

    // check BatchMapping for lazy complete schema
}
