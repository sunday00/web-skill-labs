package local.sunday00.ytbspringmongo.domain.bank.account;

import graphql.GraphQLContext;
import graphql.GraphQLError;
import graphql.GraphQLException;
import graphql.GraphqlErrorException;
import graphql.execution.DataFetcherResult;
import graphql.schema.DataFetchingEnvironment;
import local.sunday00.ytbspringmongo.domain.bank.account.dto.CreateBankAccountInputs;
import lombok.extern.slf4j.Slf4j;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.MutationMapping;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Slf4j
@Controller
public class BankAccountController {

//    @QueryMapping
//    public BankAccount bankAccount (@Argument UUID id, GraphQLContext context) {
//        log.info("Retrieving bank account");
//
//        return BankAccount
//                .builder()
//                .id(id)
//                .client(Client.builder().id(UUID.randomUUID())
//                        .firstName("Hey")
//                        .middleName(Optional.of(List.of("JJ", "JR")))
//                        .lastName("Ho")
//                        .build())
//                .currency(Currency.USD)
//                .build();
//    }

    @QueryMapping
    public DataFetcherResult<BankAccount> bankAccount (@Argument UUID id, GraphQLContext context, DataFetchingEnvironment environment) {
        log.info("Retrieving bank account");

        log.info("env {}", environment);

        return DataFetcherResult.<BankAccount>newResult()
                .data(
                        BankAccount
                                .builder()
                                .id(id)
                                .client(Client.builder().id(UUID.randomUUID())
                                        .firstName("Hey")
                                        .middleName(Optional.of(List.of("JJ", "JR")))
                                        .lastName("Ho")
                                        .build())
                                .currency(Currency.USD)
                                .build()
                )
                .error(GraphQLError.newError().message("OOOPs...").build())
                .build();
    }

    @MutationMapping
    public BankAccount createBankAccount(@Argument CreateBankAccountInputs bankAccountInputs, GraphQLContext context) {
        log.info("requesting create bank account: {}", bankAccountInputs)  ;

        return BankAccount
                .builder()
                .id(UUID.randomUUID())
                .client(Client.builder().id(UUID.randomUUID())
                        .firstName("Hey")
                        .middleName(Optional.of(List.of("JJ", "JR")))
                        .lastName("Ho")
                        .build())
                .currency(Currency.USD)
                .build();
    }
}
