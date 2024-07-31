package local.sunday00.ytbspringmongo.domain.bank.account;

import graphql.GraphQLContext;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;
import java.util.ArrayList;

@Controller
@Slf4j
public class FriendsController {
    @Cacheable(cacheNames = {"bank.friends.user"})
    @QueryMapping
    public User getUser(@Argument int id, GraphQLContext context) {
        log.info(context.get("AUTH"));

        context.put("token", "poi");

        User user1 = User.builder().id(1).name("Batman").friends(new ArrayList<>()).build();
        User user2 = User.builder().id(2).name("Superman").friends(new ArrayList<>()).build();

        user1.friends().add(user2);
        user2.friends().add(user1);

        return user1;
    }
}
