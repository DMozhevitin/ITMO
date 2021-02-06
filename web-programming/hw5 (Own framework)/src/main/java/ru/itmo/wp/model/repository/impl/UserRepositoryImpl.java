package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.database.DatabaseUtils;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.RepositoryException;
import ru.itmo.wp.model.repository.UserRepository;

import javax.sql.DataSource;
import java.sql.*;
import java.util.List;

public class UserRepositoryImpl extends BasicRepositoryImpl<User> implements UserRepository {
    private final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();
    private static final String TABLE_NAME = "User";

    @Override
    public User find(long id) {
        return super.find(id, TABLE_NAME, this::toUser);
    }

    @Override
    public User findByLogin(String login) {
        return super.findBy(login, "login", TABLE_NAME, this::toUser);
    }

    private User findByParamAndPasswordSha(String param, String paramName, String passwordSha) {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM User WHERE " + paramName+ "=? AND passwordSha=?")) {
                statement.setString(1, param);
                statement.setString(2, passwordSha);
                try (ResultSet resultSet = statement.executeQuery()) {
                    return toUser(statement.getMetaData(), resultSet);
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find User.", e);
        }
    }

    @Override
    public User findByLoginAndPasswordSha(String login, String passwordSha) {
        return findByParamAndPasswordSha(login, "login", passwordSha);
    }

    @Override
    public User findByEmailAndPasswordSha(String email, String passwordSha) {
        return findByParamAndPasswordSha(email, "email", passwordSha);
    }

    @Override
    public User findByEmail(String email) {
        return super.findBy(email, "email", TABLE_NAME, this::toUser);
    }

    @Override
    public List<User> findAll() {
        return super.findAll(TABLE_NAME, this::toUser);
    }

    @Override
    public int findCount() {
        return findAll().size();
    }

    private User toUser(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        User user = new User();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    user.setId(resultSet.getLong(i));
                    break;
                case "login":
                    user.setLogin(resultSet.getString(i));
                    break;
                case "creationTime":
                    user.setCreationTime(resultSet.getTimestamp(i));
                    break;
                case "email":
                    user.setEmail(resultSet.getString(i));
                    break;
                default:
                    // No operations.
            }
        }

        return user;
    }

    @Override
    public void save(User user, String passwordSha) {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "INSERT INTO `User` (`login`, `passwordSha`, `creationTime`, `email`) VALUES (?, ?, NOW(), ?)",
                    Statement.RETURN_GENERATED_KEYS)) {
                statement.setString(1, user.getLogin());
                statement.setString(2, passwordSha);
                statement.setString(3, user.getEmail());
                super.save(user, statement, TABLE_NAME, this::toUser);
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't save User.", e);
        }
    }
}
