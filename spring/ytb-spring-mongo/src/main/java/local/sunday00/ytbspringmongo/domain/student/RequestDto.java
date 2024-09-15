package local.sunday00.ytbspringmongo.domain.student;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class RequestDto {
    private Integer rno;
    private String name;
    private String address;
}
