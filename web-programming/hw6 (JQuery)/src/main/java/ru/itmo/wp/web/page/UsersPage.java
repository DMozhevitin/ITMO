package ru.itmo.wp.web.page;

import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

/** @noinspection unused*/
public class UsersPage extends Page{
    private final UserService userService = new UserService();

    @Override
    protected void before(HttpServletRequest request, Map<String, Object> view) {
        super.before(request, view);
        view.put("user", userService.find(getUser().getId()));
    }

    private void action(HttpServletRequest request, Map<String, Object> view) {
        if (getUser() == null) {
            throw new RedirectException("/index");
        }
    }

    private void findAll(HttpServletRequest request, Map<String, Object> view) {
        view.put("users", userService.findAll());
    }

    protected void findUser(HttpServletRequest request, Map<String, Object> view) {
        view.put("foundUser",
                userService.find(Long.parseLong(request.getParameter("userId"))));
    }

    private void checkAdmin(HttpServletRequest request, Map<String, Object> view) {
        view.put("admin", userService.find(getUser().getId()).isAdmin());
    }

    private void toggleAdmin(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        if (!userService.find(getUser().getId()).isAdmin()) {
            throw new RedirectException("/users");
        }

        long id;
        try {
            id = Long.parseLong(request.getParameter("userId"));
        } catch (NumberFormatException e) {
            throw new ValidationException("This user doesn't exist");
        }

        if (userService.find(id) == null) {
            throw new ValidationException("This user doesn't exist");
        }

        userService.toggleAdmin(id);
    }
}
