package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.TalkRepository;
import ru.itmo.wp.model.repository.UserRepository;
import ru.itmo.wp.model.repository.impl.TalkRepositoryImpl;
import ru.itmo.wp.model.repository.impl.UserRepositoryImpl;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.Map;

public class TalksPage extends Page {
    private final UserService userService = new UserService();
    private final TalkRepository talkRepository = new TalkRepositoryImpl();
    private final UserRepository userRepository = new UserRepositoryImpl();

    @Override
    protected void before(Map<String, Object> view, HttpServletRequest request) {
        super.before(view, request);
        view.put("users", userService.findAll());
        if (getUser() == null) {
            return;
        }

        Map<Long, User> userById = new HashMap<>();
        var allUsers = userRepository.findAll();

        for (var user: allUsers) {
            userById.put(user.getId(), user);
        }

        view.put("sent", talkRepository.findBySourceUserId(getUser().getId()));
        view.put("received", talkRepository.findByTargetUserId(getUser().getId()));
        view.put("userById", userById);
    }

    private void action(Map<String, Object> view) {
        if (getUser() == null) {
            setMessage("This page is avaliable only for logged users.");
            throw new RedirectException("/index");
        }
    }

    private void sendMessage(HttpServletRequest request) throws ValidationException {
        long targetId;
        try {
            targetId = Long.parseLong(request.getParameter("target"));
        } catch (NumberFormatException e) {
            throw new ValidationException("Wrong target user");
        }

        String message = request.getParameter("userMessage");

        if (getUser() == null || message == null || message.trim().isEmpty()) {
            return;
        }

        User targetUser = userRepository.find(targetId);
        if (targetUser == null) {
            throw new ValidationException("Wrong target user");
        }

        talkRepository.save(new Talk(getUser().getId(), targetId, message));
        throw new RedirectException("/talks");
    }
}
