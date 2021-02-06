package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.form.NoticeForm;
import ru.itmo.wp.service.NoticeService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class AddNoticePage extends Page {
    private final NoticeService noticeService;

    public AddNoticePage(NoticeService noticeService) {
        this.noticeService = noticeService;
    }

    @PostMapping("/addNotice")
    public String addNoticePost(@Valid @ModelAttribute("noticeForm") NoticeForm noticeForm,
                                BindingResult bindingResult) {
        if (bindingResult.hasErrors()) {
            return "AddNoticePage";
        }

        noticeService.addNotice(noticeForm);
        return "redirect:/";
    }

    @GetMapping("/addNotice")
    public String addNoticeGet(Model model, HttpSession httpSession) {
        if (getUser(httpSession) == null) {
            return "redirect:/enter";
        }

        model.addAttribute("noticeForm", new NoticeForm());
        return "AddNoticePage";
    }
}
