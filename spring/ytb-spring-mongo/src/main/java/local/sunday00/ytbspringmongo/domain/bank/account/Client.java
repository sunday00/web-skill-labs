package local.sunday00.ytbspringmongo.domain.bank.account;

import lombok.Builder;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Builder
public record Client(
        UUID id,
        String firstName,
        Optional<List<String>> middleName,
        String lastName
) {
}
