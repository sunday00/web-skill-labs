package local.sunday00.ytbspringmongo;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.scheduling.annotation.EnableAsync;

@EnableAsync
@EnableCaching
@SpringBootApplication
public class YtbSpringMongoApplication {

    public static void main(String[] args) {
        SpringApplication.run(YtbSpringMongoApplication.class, args);
    }

}
