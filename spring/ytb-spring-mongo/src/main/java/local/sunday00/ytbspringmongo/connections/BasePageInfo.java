package local.sunday00.ytbspringmongo.connections;

public record BasePageInfo (
    String startCursor,
    String endCursor,
    Boolean hasNextPage,
    Boolean hasPreviousPage
) {
}
