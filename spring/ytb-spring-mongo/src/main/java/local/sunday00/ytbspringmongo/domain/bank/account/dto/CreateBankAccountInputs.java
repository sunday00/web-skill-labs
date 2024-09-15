package local.sunday00.ytbspringmongo.domain.bank.account.dto;

import local.sunday00.ytbspringmongo.domain.bank.account.Currency;
import lombok.Builder;

@Builder
public record CreateBankAccountInputs(
        ClientInputs client,
        Currency currency
) {
}
