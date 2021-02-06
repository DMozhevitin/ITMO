package ru.itmo.wp.controller;

import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp.domain.Comment;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.exception.ValidationException;
import ru.itmo.wp.service.PostService;
import ru.itmo.wp.service.UserService;
import ru.itmo.wp.util.BindingResultUtils;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

@RestController
@RequestMapping("/api/1")
public class PostController extends ApiController {
    private final PostService postService;
    private final UserService userService;

    public PostController(PostService postService, UserService userService) {
        this.postService = postService;
        this.userService = userService;
    }

    @GetMapping("posts")
    public List<Post> findPosts() {
        return postService.findAll();
    }

    @PostMapping("posts")
    public void addPost(@Valid @RequestBody Post post,
                        BindingResult bindingResult, HttpServletRequest request) {
        if (bindingResult.hasErrors()) {
            throw new ValidationException(BindingResultUtils.getErrorMessage(bindingResult));
        }

        User user = getUser(request);
        if (user != null) {
            userService.writePost(getUser(request), post);
        }
    }

    @PostMapping("post/{id}")
    public void addComment(@PathVariable String id, @Valid @RequestBody Comment comment,
                           BindingResult bindingResult, HttpServletRequest request) {
        if (bindingResult.hasErrors()) {
            throw new ValidationException(BindingResultUtils.getErrorMessage(bindingResult));
        }

        long postId;
        try {
            postId = Long.parseLong(id);
        } catch (NumberFormatException ignored) {
            throw new ValidationException("Wrong post id");
        }

        Post post = postService.findById(postId);
        User user = getUser(request);

        if (user != null && post != null) {
            postService.writeComment(post, comment, user);
        }
    }

    @GetMapping("comments/{id}")
    public List<Comment> getComments(@PathVariable String id) {
        long postId;
        try {
            postId = Long.parseLong(id);
        } catch (NumberFormatException ignored) {
            throw new ValidationException("Wrong post id");
        }

        Post post = postService.findById(postId);
        return post.getComments();
    }

    @GetMapping("post/{id}")
    public Post getPost(@PathVariable String id) {
        long postId;
        try {
            postId = Long.parseLong(id);
        } catch (NumberFormatException ignored) {
            throw new ValidationException("Wrong post id");
        }

        return postService.findById(postId);
    }
}
