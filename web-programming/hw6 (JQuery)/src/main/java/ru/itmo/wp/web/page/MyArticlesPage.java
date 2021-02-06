package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.ArticleRepository;
import ru.itmo.wp.model.repository.impl.ArticleRepositoryImpl;
import ru.itmo.wp.model.service.ArticleService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Map;

/** @noinspection unused*/
public class MyArticlesPage extends Page {
    private final ArticleRepository articleRepository = new ArticleRepositoryImpl();
    private final ArticleService articleService = new ArticleService();

    private void action(HttpServletRequest request, Map<String, Object> view) {
        if (getUser() == null) {
            throw new RedirectException("/index");
        }

        List<Article> userArticles = articleRepository.findByUserId(getUser().getId());
        view.put("userArticles", userArticles);
    }

    private void toggleHidden(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        long articleId;
        try {
            articleId = Long.parseLong(request.getParameter("articleId"));
        } catch (NumberFormatException e) {
            throw new ValidationException("No such post");
        }

        Article article = articleRepository.find(articleId);

        if (article == null || article.getUserId() != getUser().getId()) {
            throw new RedirectException("/index");
        }

        String value = request.getParameter("value");
        if (value == null) {
            throw new ValidationException("Invalid value of 'hidden' parameter");
        }

        articleService.toggleHidden(articleId, value.equals("Hide"));
    }
}
