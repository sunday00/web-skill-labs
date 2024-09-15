package local.sunday00.ytbspringmongo.domain.bank.account;

import lombok.Builder;

import java.util.List;

@Builder
public record User(
        int id,
        String name,
        List<User> friends
) {
}
