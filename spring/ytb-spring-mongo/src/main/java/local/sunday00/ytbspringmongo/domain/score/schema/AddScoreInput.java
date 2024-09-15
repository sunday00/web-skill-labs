package local.sunday00.ytbspringmongo.domain.score.schema;

import jakarta.validation.constraints.*;
import lombok.Data;

@Data
public class AddScoreInput {
    @NotNull
    private String name;

    @NotNull
    @Max(value = 100, message = "Oops... No more 100")
    private Integer score;
}
