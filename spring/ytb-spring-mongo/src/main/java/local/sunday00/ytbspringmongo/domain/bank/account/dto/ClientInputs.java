package local.sunday00.ytbspringmongo.domain.bank.account.dto;

import lombok.Builder;

import java.util.List;
import java.util.Optional;

@Builder
public record ClientInputs(
        String firstName,
        Optional<List<String>> middleName,
        String lastName
) {
}
