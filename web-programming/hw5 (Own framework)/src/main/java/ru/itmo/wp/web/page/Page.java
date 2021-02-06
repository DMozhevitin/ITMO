package ru.itmo.wp.web.page;

import com.google.common.base.Strings;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.service.UserService;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public abstract class Page {
    private UserService userService = new UserService();
    private HttpServletRequest request;

    private void action() {
        //No operation
    }

    protected void before(Map<String, Object> view, HttpServletRequest request) {
        this.request = request;

        putMessage(request, view);

        User user = getUser();
        if (user != null) {
            view.put("user", user);
        }

        view.put("userCount", userService.findCount());
    }

    private void putMessage(HttpServletRequest request, Map<String, Object> view) {
        String message = getMessage();
        if (!Strings.isNullOrEmpty(message)) {
            view.put("message", message);
            removeMessage();
        }
    }

    protected User getUser() {
        return (User) request.getSession().getAttribute("user");
    }

    protected void setUser(User user) {
        request.getSession().setAttribute("user", user);
    }

    protected void removeUser() {
        request.getSession().removeAttribute("user");
    }

    protected void setMessage(String message) {
        request.getSession().setAttribute("message", message);
    }

    protected void removeMessage() {
        request.getSession().removeAttribute("message");
    }

    protected String getMessage() {
        return (String) request.getSession().getAttribute("message");
    }

    protected void after(HttpServletRequest request, Map<String, Object> view) {
        //No operation.
    }
}
