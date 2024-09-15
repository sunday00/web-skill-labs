package local.sunday00.ytbspringmongo.interceptors;

import graphql.GraphQLError;
import local.sunday00.ytbspringmongo.event.ExampleEmitter;
import local.sunday00.ytbspringmongo.event.ExampleEvent;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.graphql.server.WebGraphQlInterceptor;
import org.springframework.graphql.server.WebGraphQlRequest;
import org.springframework.graphql.server.WebGraphQlResponse;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.Collections;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class RequestHeaderInterceptor implements WebGraphQlInterceptor {
    private final ExampleEmitter exampleEmitter;

    @Override
    @NonNull
    public Mono<WebGraphQlResponse> intercept(WebGraphQlRequest request, Chain chain) {
        this.exampleEmitter.emit(new ExampleEvent("EVENT EMITTED"));

        String value = request.getHeaders().getFirst("AUTH");
        log.info("value: {}", value);

        if(value == null) return chain.next(request);

        request.configureExecutionInput((executionInput, builder) ->
                builder.graphQLContext(Collections.singletonMap("token", value)).build());

        return chain.next(request)
                .map(res -> {
                    if(res.isValid()) return res;
                    else {

                        log.info("{}", res.getErrors());

                        return res.transform(builder -> builder.errors(
                                        List.of(
                                                GraphQLError.newError()
                                                        .message("validationEEEE")
                                                        .build()
                                        )
                                ).build()
                        );
                    }
        });
    }
}
