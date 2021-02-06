package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.database.DatabaseUtils;
import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.exception.RepositoryException;
import ru.itmo.wp.model.repository.ArticleRepository;

import javax.sql.DataSource;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class ArticleRepositoryImpl extends BasicRepositoryImpl<Article> implements ArticleRepository {
    private final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();
    private static final String TABLE_NAME = "Article";

    @Override
    public Article find(long id) {
        return super.find(id, TABLE_NAME, this::toArticle);
    }

    @Override
    public void save(Article article) {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "INSERT INTO `Article` (`userId`, `title`, `text`, `creationTime`, `hidden`) VALUES (?, ?, ?, NOW(), ?)",
                    Statement.RETURN_GENERATED_KEYS)) {
                statement.setLong(1, article.getUserId());
                statement.setString(2, article.getTitle());
                statement.setString(3, article.getText());
                statement.setBoolean(4, false);
                super.save(article, statement, TABLE_NAME, this::toArticle);
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't save article.", e);
        }
    }



    @Override
    public List<Article> findByUserId(long userId) {
        return super.findBy(userId, "userId", TABLE_NAME, this::toArticle);
    }

    @Override
    public List<Article> findNotHidden() {
        List<Article> res = new ArrayList<>();
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM `Article` WHERE hidden=FALSE ORDER BY id DESC")) {
                try (ResultSet resultSet = statement.executeQuery()) {
                    Article article;
                    while ((article = toArticle(statement.getMetaData(), resultSet)) != null) {
                        res.add(article);
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find User.", e);
        }
        return res;
    }

    @Override
    public void updateHidden(long id, boolean hiddenValue) {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "UPDATE `Article` SET `hidden`=" + String.valueOf(hiddenValue).toUpperCase() + " WHERE id=" + id,
                    Statement.RETURN_GENERATED_KEYS)) {
                if (statement.executeUpdate() != 1) {
                    throw new RepositoryException("Can't update Article.");
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't update article.", e);
        }
    }

    @Override
    public List<Article> findAll() {
        return super.findAll(TABLE_NAME, this::toArticle);
    }

    private Article toArticle(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        Article article = new Article();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    article.setId(resultSet.getLong(i));
                    break;
                case "userId":
                    article.setUserId(resultSet.getLong(i));
                    break;
                case "title":
                    article.setTitle(resultSet.getString(i));
                    break;
                case "text":
                    article.setText(resultSet.getString(i));
                    break;
                case "creationTime":
                    article.setCreationTime(resultSet.getTimestamp(i));
                    break;
                case "hidden":
                    article.setHidden(resultSet.getBoolean(i));
                    break;
                default:
                    //No operations.
            }
        }

        return article;
    }
}
