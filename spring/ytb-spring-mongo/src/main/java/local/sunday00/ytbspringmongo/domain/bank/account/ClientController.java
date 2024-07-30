package local.sunday00.ytbspringmongo.domain.bank.account;

import graphql.GraphQLContext;
import lombok.extern.slf4j.Slf4j;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Slf4j
@Controller
public class ClientController {
    private final ExecutorService executorService = Executors.newFixedThreadPool(
        Runtime.getRuntime().availableProcessors()
    );

    @QueryMapping
    public CompletableFuture<Client> fetchOneClient(@Argument Integer id, GraphQLContext context) {
        log.info("1");

        return CompletableFuture.supplyAsync(() -> {
                log.info("2");

                return Client.builder()
                        .id(UUID.randomUUID())
                        .firstName("Hey")
                        .lastName("Ho")
                        .build();
            }
        , this.executorService);
    }
}
