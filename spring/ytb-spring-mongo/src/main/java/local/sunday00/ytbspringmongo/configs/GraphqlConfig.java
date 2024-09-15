package local.sunday00.ytbspringmongo.configs;

import graphql.analysis.MaxQueryDepthInstrumentation;
import graphql.execution.preparsed.PreparsedDocumentProvider;
import graphql.execution.preparsed.persisted.ApolloPersistedQuerySupport;
import graphql.execution.preparsed.persisted.InMemoryPersistedQueryCache;
import graphql.scalars.ExtendedScalars;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.graphql.GraphQlSourceBuilderCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.graphql.execution.RuntimeWiringConfigurer;

import java.util.Collections;

@Slf4j
@Configuration
public class GraphqlConfig {
    @Bean
    public RuntimeWiringConfigurer runtimeWiringConfigurer() {
        return wiringBuilder -> wiringBuilder
                .scalar(GraphqlCustomSchema.DateScalar)
                .scalar(ExtendedScalars.GraphQLBigDecimal);
    }

    @Bean
    public MaxQueryDepthInstrumentation maxQueryDepthInstrumentation() {
        // 최대 깊이를 10으로 설정 (필요에 따라 변경)
        return new MaxQueryDepthInstrumentation(15);
    }

    @Bean
    GraphQlSourceBuilderCustomizer inspectionCustomizer() {
//        PreparsedDocumentProvider provider =
//                new ApolloPersistedQuerySupport(
//                        new InMemoryPersistedQueryCache(Collections.emptyMap())
//                );

        return source ->
                source.inspectSchemaMappings(report -> log.info(report.toString()))
//                        .configureGraphQl(builder ->
//                            builder.preparsedDocumentProvider(provider)
//                        )
                ;
    }
}
