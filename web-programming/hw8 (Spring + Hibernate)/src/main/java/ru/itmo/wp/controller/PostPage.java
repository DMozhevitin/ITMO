package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.domain.Comment;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.security.Guest;
import ru.itmo.wp.service.PostService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class PostPage extends Page {
    private final PostService postService;

    public PostPage(PostService postService) {
        this.postService = postService;
    }

    @Guest
    @GetMapping("/post/{id}")
    public String post(@PathVariable String id, Model model) {
        long postId = parsePostId(id);

        Post viewedPost = postId == -1 ? null : postService.findById(postId);
        model.addAttribute("viewedPost", viewedPost);
        model.addAttribute("commentForm", new Comment());
        return "PostPage";
    }

    @PostMapping("/post/{id}")
    public String post(@PathVariable String id,
                       @Valid @ModelAttribute("commentForm") Comment comment,
                       HttpSession httpSession) {
        long postId = parsePostId(id);

        comment.setUser(getUser(httpSession));
        Post post = postService.findById(postId);

        if (post != null) {
            postService.addComment(post, comment);
        }

        return "redirect:/post/" + id;
    }

    private long parsePostId(String id) {
        long postId = -1;
        try {
            postId = Long.parseLong(id);
        } catch (NumberFormatException ignored) { }

        return postId;
    }
}
