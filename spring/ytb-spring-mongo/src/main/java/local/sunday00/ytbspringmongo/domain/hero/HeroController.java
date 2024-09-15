package local.sunday00.ytbspringmongo.domain.hero;

import graphql.GraphQLContext;
import graphql.schema.DataFetchingEnvironment;
import graphql.schema.SelectedField;

import local.sunday00.ytbspringmongo.connections.BaseEdge;
import local.sunday00.ytbspringmongo.connections.BasePageInfo;
import local.sunday00.ytbspringmongo.domain.hero.schema.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.graphql.data.query.ScrollSubrange;
import org.springframework.stereotype.Controller;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Slf4j
@RequiredArgsConstructor
@Controller
public class HeroController {
    @QueryMapping
    public Hero hero (@Argument String name, GraphQLContext context, DataFetchingEnvironment e) {
        log.info("""
                
                =====================
                
                HeroController: {}
                
                =====================
                """,
                e.getSelectionSet().getFields().stream()
                        .map(SelectedField::getName)
                        .collect(Collectors.toUnmodifiableSet())
        );
//        log.info("HeroController: {} \n\n", e.getSelectionSet().getFields());

        return Hero.builder()
                .name(name)
                .mind(HeroSide.Good)
                .ability(Optional.of(HeroSkill.Fly))
                .build();
    }

    @QueryMapping
//    public Window<Hero> pagingHeroList(ScrollSubrange subrange, GraphQLContext context) {
//    public List<Hero> pagingHeroList(ScrollSubrange subrange, GraphQLContext context) {
//    public Connection<Hero> pagingHeroList(ScrollSubrange subrange, GraphQLContext context) {
    public HeroConnection pagingHeroList(ScrollSubrange subrange, GraphQLContext context) {
//    public HeroConnection pagingHeroList(@Argument("first") Integer first,
//                                         @Argument("after") String after,
//                                         GraphQLContext context) {
//    public PageHero pagingHeroList(GraphQLContext context, DataFetchingEnvironment e) {
//        ScrollPosition scrollPosition = subrange.position().orElse(ScrollPosition.offset());
//        Limit limit = Limit.of(subrange.count().orElse(10));
//        Sort sort = Sort.by("title").ascending();

        log.info("nfniowenoenwaocero");
        log.info("{}", subrange.position());

//        var pageInfo = new DefaultPageInfo(
//                new DefaultConnectionCursor(""),
//                new DefaultConnectionCursor(""),
//                false,
//                true
//        );

//        return heroService.pagedHeroList(scrollPosition, limit, sort);
//        return new DefaultConnection<>(FakeHeroes.heroList(), pageInfo);
//        return new HeroConnection(FakeHeroes.heroList(), pageInfo);

//        PageHero pageHero = new PageHero();
//        pageHero.setItems(
//                new DefaultConnection<>(FakeHeroes.heroList(), pageInfo)
//        );

//        pageHero.setItems(this.items(pageHero, ScrollSubrange.create(ScrollPosition.offset(), 1, true)));

//        ScrollPosition sp = ScrollPosition.of(new HashMap<>(), ScrollPosition.Direction.FORWARD);
//        ScrollSubrange ss = ScrollSubrange.create(sp, 10, true);
//        pageHero.setItems(this.items(pageHero, ss).toSet());

//        return pageHero;

//        ScrollPosition scrollPosition = subrange.position().orElse(ScrollPosition.offset());
//        IntFunction<ScrollPosition> f = n -> scrollPosition;
//
//        return Window.<Hero>from(
//                List.of(
//                        Hero.builder().name("bb").mind(HeroSide.Good).build(),
//                        Hero.builder().name("dd").mind(HeroSide.Good).build()
//                ), f);

//        ConnectionCursor sc = new DefaultConnectionCursor("ba");
//        ConnectionCursor ec = new DefaultConnectionCursor("ba");
        log.info("example cursor :::::: {}, {}, {}",
                Base64.getEncoder().encodeToString(String.valueOf("O_1").getBytes(StandardCharsets.UTF_8)),
                Base64.getEncoder().encodeToString(String.valueOf("O_2").getBytes(StandardCharsets.UTF_8)),
                Base64.getEncoder().encodeToString(String.valueOf("O_10").getBytes(StandardCharsets.UTF_8))
                );

        return new HeroConnection(
                List.of(
                        new BaseEdge<Hero>("T18x", FakeHeroes.heroList().getFirst()),
                        new BaseEdge<Hero>("T18z", FakeHeroes.heroList().get(1))
                ),
                new BasePageInfo("T18x", "T18z", true, false)
        );
    }

//    @SchemaMapping
//    public Window<Hero> items(PageHero pageHero, ScrollSubrange subrange) {
//        ScrollPosition scrollPosition = subrange.position().orElse(ScrollPosition.offset());
//        Limit limit = Limit.of(subrange.count().orElse(10));
//        Sort sort = Sort.by("title").ascending();
//
//        IntFunction<ScrollPosition> f = n -> scrollPosition;
//
//        log.info("sp: {}", scrollPosition);
//
//        return Window.<Hero>from(
//                List.of(
//                        Hero.builder().name("bb").mind(HeroSide.Good).build(),
//                        Hero.builder().name("dd").mind(HeroSide.Good).build()
//                ), f);
//    }
}
