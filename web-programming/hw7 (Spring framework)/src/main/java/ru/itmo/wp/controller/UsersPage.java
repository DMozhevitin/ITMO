package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.form.ToggleDisabledForm;
import ru.itmo.wp.service.UserService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class UsersPage extends Page {
    private final UserService userService;

    public UsersPage(UserService userService) {
        this.userService = userService;
    }

    @GetMapping("/users/all")
    public String users(Model model) {
        model.addAttribute("toggleDisabledForm", new ToggleDisabledForm());
        model.addAttribute("users", userService.findAll());
        return "UsersPage";
    }

    @PostMapping("/users/all")
    public String toggleDisabled(@Valid @ModelAttribute("toggleDisabledForm")ToggleDisabledForm toggleDisabledForm,
                                 BindingResult bindingResult,
                                 HttpSession httpSession) {

        if (bindingResult.hasErrors()) {
            return "UsersPage";
        }

        if (getUser(httpSession) == null) {
            return "redirect:/enter";
        }

        User user = userService.findById(toggleDisabledForm.getId());

        if (user == null) {
            return "redirect:/";
        }

        userService.toggleDisabled(toggleDisabledForm);
        return "redirect:/users/all";
    }
}
