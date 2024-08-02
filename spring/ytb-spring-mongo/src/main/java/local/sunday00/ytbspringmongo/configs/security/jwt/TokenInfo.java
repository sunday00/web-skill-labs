package local.sunday00.ytbspringmongo.configs.security.jwt;

public record TokenInfo (
        String accessToken,
        String refreshToken
) {
}
