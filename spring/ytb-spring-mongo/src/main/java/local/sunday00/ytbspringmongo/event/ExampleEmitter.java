package local.sunday00.ytbspringmongo.event;

import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class ExampleEmitter {
    private final ApplicationEventPublisher eventPublisher;

    public void emit(ExampleEvent example) {
        eventPublisher.publishEvent(example);
    }
}
