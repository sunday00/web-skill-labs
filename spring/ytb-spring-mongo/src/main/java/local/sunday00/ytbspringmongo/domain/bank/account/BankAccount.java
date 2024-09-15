package local.sunday00.ytbspringmongo.domain.bank.account;

import lombok.Builder;

import java.util.UUID;

@Builder
public record BankAccount (
    UUID id,
    Client client,
    Currency currency
) {}
