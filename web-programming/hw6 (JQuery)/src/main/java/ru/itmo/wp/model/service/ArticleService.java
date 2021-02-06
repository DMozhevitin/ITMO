package ru.itmo.wp.model.service;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.repository.ArticleRepository;
import ru.itmo.wp.model.repository.impl.ArticleRepositoryImpl;

public class ArticleService {
    private final ArticleRepository articleRepository = new ArticleRepositoryImpl();

    public void toggleHidden(long articleId, boolean value) {
        Article article = articleRepository.find(articleId);

        if (article != null) {
            articleRepository.updateHidden(articleId, value);
        }
    }
}
