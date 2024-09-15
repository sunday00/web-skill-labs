package local.sunday00.ytbspringmongo.domain.hero.schema;

import graphql.relay.DefaultConnectionCursor;
import graphql.relay.DefaultEdge;
import graphql.relay.Edge;

import java.util.List;

public class FakeHeroes {
//    public static List<Edge<Hero>> heroList () {
    public static List<Hero> heroList () {
//        Edge<Hero> hero1 = new DefaultEdge<>(
//                Hero.builder()
//                .name("Batman")
//                .mind(HeroSide.Good)
//                .build(),
//                new DefaultConnectionCursor("")
//        );
//
//        Edge<Hero> hero2 = new DefaultEdge<>(
//                Hero.builder()
//                .name("Superman")
//                .mind(HeroSide.Good)
//                .build(),
//                new DefaultConnectionCursor("")
//        );
//
//        Edge<Hero> hero3 = new DefaultEdge<>(
//                Hero.builder()
//                        .name("Joker")
//                        .mind(HeroSide.Evil)
//                        .build(),
//                new DefaultConnectionCursor("")
//        );

        Hero hero4 = Hero.builder()
                .name("IronMan")
                .mind(HeroSide.Good)
                .build();

        Hero hero5 = Hero.builder()
                .name("CaptainAmerica")
                .mind(HeroSide.Good)
                .build();

        Hero hero6 = Hero.builder()
                .name("WinterSoldier")
                .mind(HeroSide.Natural)
                .build();

//        return List.of(hero1, hero2, hero3);
        return List.of(hero4, hero5, hero6);
    }
}
