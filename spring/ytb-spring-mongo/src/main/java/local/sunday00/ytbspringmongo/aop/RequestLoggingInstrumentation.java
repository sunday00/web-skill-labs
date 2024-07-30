package local.sunday00.ytbspringmongo.aop;

import graphql.ExecutionResult;
import graphql.execution.instrumentation.InstrumentationContext;
import graphql.execution.instrumentation.InstrumentationState;
import graphql.execution.instrumentation.SimpleInstrumentationContext;
import graphql.execution.instrumentation.SimplePerformantInstrumentation;
import graphql.execution.instrumentation.parameters.InstrumentationExecutionParameters;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.Nullable;
import org.slf4j.MDC;
import org.springframework.stereotype.Component;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;

@Slf4j
@Component
@RequiredArgsConstructor
public class RequestLoggingInstrumentation extends SimplePerformantInstrumentation {
    private static final String CORRELATION_ID = "correlationId";
    private final Clock clock;

    @Override
    public @Nullable InstrumentationContext<ExecutionResult> beginExecution(InstrumentationExecutionParameters parameters, InstrumentationState state) {
        var start = Instant.now(clock);
        var executionId = parameters.getExecutionInput().getExecutionId();
        MDC.put(CORRELATION_ID, executionId.toString());
        //https://www.youtube.com/watch?v=17AFe2eCRqc&list=PLiwhu8iLxKwL1TU0RMM6z7TtkyW-3-5Wi&index=27
        // if correlationId propagation is not enough, see this video for propagation.

        log.info("que: {}, var: {}", parameters.getQuery(), parameters.getVariables());

        return SimpleInstrumentationContext.whenCompleted((executionResult, throwable) -> {
            var duration = Duration.between(start, Instant.now(clock));

            if(throwable == null) {
                log.info("duration: {} ms", duration.toMillis());
            } else {
                log.warn("failed in : {}", duration, throwable);
            }
        });
    }
}
