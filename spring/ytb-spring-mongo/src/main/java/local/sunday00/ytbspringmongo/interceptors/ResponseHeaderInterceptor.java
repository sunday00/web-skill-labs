package local.sunday00.ytbspringmongo.interceptors;

import graphql.GraphQLError;
import graphql.GraphqlErrorBuilder;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.graphql.ResponseError;
import org.springframework.graphql.execution.ErrorType;
import org.springframework.graphql.server.WebGraphQlInterceptor;
import org.springframework.graphql.server.WebGraphQlRequest;
import org.springframework.graphql.server.WebGraphQlResponse;
//import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@Component
public class ResponseHeaderInterceptor implements WebGraphQlInterceptor {

    @Override
    @NonNull
    public Mono<WebGraphQlResponse> intercept(WebGraphQlRequest request, Chain chain) {
        return chain.next(request).doOnNext(response -> {
//            List<GraphQLError> errors = response.getErrors().stream()
//                    .map(error -> GraphqlErrorBuilder.newError()
//                            .message("Request error. Ensure request is valid and try again.")
//                            .errorType(ErrorType.BAD_REQUEST)
//                            .build()).toList();
//
//            if(errors.getFirst().getMessage() != null) {
//                response.transform(builder -> builder.errors(errors).build());
//                return;
//            }

//            List<ResponseError> errors = response.getErrors().stream().filter(GraphQLError.class::isInstance).toList();

//            response.getErrors().clear();
//            response.getErrors().add(response.getErrors().getFirst());

            String value = response.getExecutionInput().getGraphQLContext().get("token");

            log.info(value);

            if(value != null) {
//            ResponseCookie cookie = ResponseCookie.from("token", value).build();
//            response.getResponseHeaders().add(HttpHeaders.SET_COOKIE, cookie.toString());
                response.getResponseHeaders().add("CUSTOM", value);
            }
        });
    }
}
