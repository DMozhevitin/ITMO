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
import java.util.Map;

/** @noinspection unused*/
public class EnterPage extends Page {
    private final UserService userService = new UserService();
    private final EventRepository eventRepository = new EventRepositoryImpl();

    private void enter(HttpServletRequest request) throws ValidationException {
        String loginOrEmail = request.getParameter("loginOrEmail");
        String password = request.getParameter("password");

        User user = userService.validateAndFindByLoginCredentials(loginOrEmail, password);
        setUser(user);
        setMessage("Hello, " + user.getLogin());
        eventRepository.save(new Event(user.getId(), EventType.ENTER));
        throw new RedirectException("/index");
    }
}
