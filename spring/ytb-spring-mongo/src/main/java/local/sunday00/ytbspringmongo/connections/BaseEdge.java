package local.sunday00.ytbspringmongo.connections;

public record BaseEdge<T>(
        String cursor,
        T node
) {
}
