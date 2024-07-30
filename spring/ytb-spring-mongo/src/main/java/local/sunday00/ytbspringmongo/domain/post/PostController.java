package local.sunday00.ytbspringmongo.domain.post;


import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

import java.util.ArrayList;
import java.util.List;

@Controller
public class PostController {

    @QueryMapping
    public List<Post> recentPosts(@Argument int count, @Argument int offset) {
        Post post = new Post();
        post.setId("1");
        post.setCategory("dev");
        post.setTitle("hi");

        List<Post> posts = new ArrayList<>();
        posts.add(post);

        return posts;
    }
}
