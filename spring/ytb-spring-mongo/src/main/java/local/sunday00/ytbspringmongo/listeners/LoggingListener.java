package local.sunday00.ytbspringmongo.listeners;

import local.sunday00.ytbspringmongo.event.ExampleEvent;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

@Slf4j
@Component
public class LoggingListener {

    @EventListener(ExampleEvent.class)
    @Async("propagatingContextExecutor")
    public void emailReceived(ExampleEvent event) {
        // asynchronously process the received event
        // this logging statement will contain the expected MDC entries from the propagated context
        log.info(event.getMessage());
    }

}
