package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.domain.Role;
import ru.itmo.wp.form.PostForm;
import ru.itmo.wp.form.validator.PostFormValidator;
import ru.itmo.wp.security.AnyRole;
import ru.itmo.wp.service.UserService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class WritePostPage extends Page {
    private final UserService userService;
    private final PostFormValidator postFormValidator;

    public WritePostPage(UserService userService, PostFormValidator postFormValidator) {
        this.userService = userService;
        this.postFormValidator = postFormValidator;
    }

    @AnyRole({Role.Name.WRITER, Role.Name.ADMIN})
    @GetMapping("/writePost")
    public String writePostGet(Model model) {
        model.addAttribute("postForm", new PostForm());
        return "WritePostPage";
    }

    @InitBinder("postForm")
    public void initBinder(WebDataBinder binder) {
        binder.addValidators(postFormValidator);
    }

    @AnyRole({Role.Name.WRITER, Role.Name.ADMIN})
    @PostMapping("/writePost")
    public String writePostPost(@Valid @ModelAttribute("postForm") PostForm postForm,
                                BindingResult bindingResult,
                                HttpSession httpSession) {
        if (bindingResult.hasErrors()) {
            return "WritePostPage";
        }
        userService.writePost(getUser(httpSession), postForm);
        putMessage(httpSession, "You published new post");
        return "redirect:/posts";
    }
}
