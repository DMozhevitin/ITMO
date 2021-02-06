package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.domain.EventType;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.EventRepository;
import ru.itmo.wp.model.repository.impl.EventRepositoryImpl;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import javax.sql.DataSource;
import java.util.Map;

/** @noinspection unused*/
public class LogoutPage extends Page {
    private final EventRepository eventRepository = new EventRepositoryImpl();
    private void action(HttpServletRequest request) {
        eventRepository.save(new Event(getUser().getId(), EventType.LOGOUT));
        removeUser();
        setMessage("Good bye. Hope to see you soon!");
        throw new RedirectException("/index");
    }
}
