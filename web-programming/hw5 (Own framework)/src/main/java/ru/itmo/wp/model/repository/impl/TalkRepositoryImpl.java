package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.database.DatabaseUtils;
import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.exception.RepositoryException;
import ru.itmo.wp.model.repository.TalkRepository;

import javax.sql.DataSource;
import java.sql.*;
import java.util.List;

public class TalkRepositoryImpl extends BasicRepositoryImpl<Talk> implements TalkRepository {
    private final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();
    private static final String TABLE_NAME = "Talk";

    @Override
    public void save(Talk talk) {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "INSERT INTO `Talk` (`sourceUserId`, `targetUserId`, `message`) VALUES (?, ?, ?)",
                    Statement.RETURN_GENERATED_KEYS)) {
                statement.setLong(1, talk.getSourceUserId());
                statement.setLong(2, talk.getTargetUserId());
                statement.setString(3, talk.getMessage());
                super.save(talk, statement, TABLE_NAME, this::toTalk);
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't save User.", e);
        }
    }

    @Override
    public Talk find(long id) {
        return super.find(id, TABLE_NAME, this::toTalk);
    }

    @Override
    public List<Talk> findBySourceUserId(long sourceUserId) {
        return super.findBy(sourceUserId, "sourceUserId", TABLE_NAME, this::toTalk);
    }

    @Override
    public List<Talk> findByTargetUserId(long targetUserId) {
        return super.findBy(targetUserId, "targetUserId", TABLE_NAME, this::toTalk);
    }

    private Talk toTalk(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        Talk talk = new Talk();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    talk.setId(resultSet.getLong(i));
                    break;
                case "sourceUserId":
                    talk.setSourceUserId(resultSet.getLong(i));
                    break;
                case "targetUserId":
                    talk.setTargetUserId(resultSet.getLong(i));
                    break;
                case "type":
                    talk.setCreationTime(resultSet.getTimestamp(i));
                    break;
                case "message":
                    talk.setMessage(resultSet.getString(i));
                    break;
                case "creationTime":
                    talk.setCreationTime(resultSet.getTimestamp(i));
                default:
                    // No operations.
            }
        }

        return talk;
    }
}
