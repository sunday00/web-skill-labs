package local.sunday00.ytbspringmongo.domain.auth;

import local.sunday00.ytbspringmongo.configs.security.jwt.JwtUtils;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

@Slf4j
@Controller
@AllArgsConstructor
public class AuthController {
    private JwtUtils jwtUtils;

    @QueryMapping
    public Auth login (@Argument Integer id, @Argument String password) {
        String token = this.jwtUtils.generateJwtToken(new User());

        log.info("token: {}", token);

        Auth auth = new Auth();
        auth.setAccessToken(token);

        return auth;
    }
}
