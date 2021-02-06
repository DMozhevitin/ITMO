package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.ArticleRepository;
import ru.itmo.wp.model.repository.impl.ArticleRepositoryImpl;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

/** @noinspection unused*/
public class ArticlePage extends Page {
    private final ArticleRepository articleRepository = new ArticleRepositoryImpl();

    private void action(HttpServletRequest request, Map<String, Object> view) {
        if (getUser() == null) {
            throw new RedirectException("/index");
        }
    }

    private void addArticle(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        String title = request.getParameter("title");
        if (title.length() > 255) {
            throw new ValidationException("Title is too long");
        }

        String text = request.getParameter("text");
        if (text.length() > 1000) {
            throw new ValidationException("Text is too long");
        }

        articleRepository.save(new Article(getUser().getId(), title, text));
        throw new RedirectException("/index");
    }
}
