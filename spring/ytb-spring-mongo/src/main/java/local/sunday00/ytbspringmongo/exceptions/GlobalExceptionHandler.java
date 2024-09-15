package local.sunday00.ytbspringmongo.exceptions;

import graphql.ErrorClassification;
import graphql.GraphQLContext;
import graphql.GraphQLError;
import graphql.GraphQLException;
import graphql.schema.DataFetchingEnvironment;
import jakarta.validation.ConstraintViolationException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.graphql.data.method.annotation.GraphQlExceptionHandler;
import org.springframework.security.authorization.AuthorizationDeniedException;
import org.springframework.web.bind.annotation.ControllerAdvice;

import java.util.*;

@Slf4j
@ControllerAdvice
public class GlobalExceptionHandler {

    @GraphQlExceptionHandler
    public GraphQLError handle(Exception ex, DataFetchingEnvironment ev) {
//        return GraphQLError.newError().errorType(ErrorType.BAD_REQUEST).message("...").build();
//        return GraphQLError.newError().errorType(CustomErrorType.FORCED_INTENDED_ERROR).message(ex.getMessage()).build();

        log.error("EX: {}", ex.getMessage());
        log.error("EX: {}", Arrays.stream(ex.getStackTrace()).toList());
        log.error("EX: {}", ex.getClass().getName());

        var className = ex.getClass().getName();

//        log.info("path: {}", Optional.ofNullable(context.get("path")));
        log.info("path: {}", ev.getExecutionStepInfo().getPath());
        log.info("locations: {}", ev.getField().getSourceLocation());

        if(className.equals(AuthorizationDeniedException.class.getName())) {
            return GraphQLError.newError()
                    .message("unauthorized")
                    .path(ev.getExecutionStepInfo().getPath())
                    .location(ev.getField().getSourceLocation())
                    .errorType(CustomErrorType.UNAUTHORIZED)
//                    .extensions(ex.getEx)
                    .build();
        }

        return GraphQLError.newError().errorType(CustomErrorType.FORCED_INTENDED_ERROR).message("??").build();
    }

    @GraphQlExceptionHandler
    public GraphQLError validationExceptionHandle(ConstraintViolationException ex, DataFetchingEnvironment ev) {
        log.error("EX: {}", ex.getMessage());
        log.error("EX: {}", Arrays.stream(ex.getStackTrace()).toList());
        log.error("EX: {}", ex.getClass().getName());

        Map<String , Object> extension = new HashMap<>();
        extension.put("code", 400);
        extension.put("message", "InputError");

        return GraphQLError.newError()
                .message(ex.getMessage())
                .path(ev.getExecutionStepInfo().getPath())
                .location(ev.getField().getSourceLocation())
                .errorType(CustomErrorType.BAD_REQUEST)
                .extensions(extension)
                .build();
    }
}
