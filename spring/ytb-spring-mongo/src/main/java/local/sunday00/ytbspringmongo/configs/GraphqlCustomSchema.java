package local.sunday00.ytbspringmongo.configs;

import graphql.GraphQLContext;
import graphql.schema.Coercing;
import graphql.schema.CoercingSerializeException;
import graphql.schema.GraphQLScalarType;
import org.jetbrains.annotations.NotNull;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Locale;

public class GraphqlCustomSchema {
    private GraphqlCustomSchema() {}

    public static final GraphQLScalarType DateScalar = GraphQLScalarType.newScalar()
            .name("YYYYMMDD")
            .description("A custom scalar that handles Date")
            .coercing(
                new Coercing() {
                    @Override
                    public Object serialize(@NotNull Object dataFetcherResult, @NotNull GraphQLContext graphQLContext, @NotNull Locale locale) throws CoercingSerializeException {
                        try {
                            LocalDateTime publishedTime = (LocalDateTime) dataFetcherResult;
//                            DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE.withZone(ZoneId.systemDefault());
                            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
                            return formatter.format(publishedTime);
                        } catch (CoercingSerializeException exception) {
                            throw new CoercingSerializeException("Invalid Input:" + exception.getMessage());
                        }
                    }
                }
            ).build();
}
