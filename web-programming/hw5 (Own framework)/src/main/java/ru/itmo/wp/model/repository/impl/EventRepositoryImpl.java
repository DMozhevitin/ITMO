package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.database.DatabaseUtils;
import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.domain.EventType;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.RepositoryException;
import ru.itmo.wp.model.repository.EventRepository;

import javax.sql.DataSource;
import java.sql.*;

public class EventRepositoryImpl extends BasicRepositoryImpl<Event> implements EventRepository {
    private final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();
    private static final String TABLE_NAME = "Event";

    public Event find(long id) {
        return super.find(id, TABLE_NAME, this::toEvent);
    }

    @Override
    public void save(Event event) {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "INSERT INTO `Event` (`userId`, `type`, `creationTime`) VALUES (?, ?, NOW())",
                    Statement.RETURN_GENERATED_KEYS)) {
                statement.setLong(1, event.getUserId());
                statement.setString(2, event.getType().toString());
                super.save(event, statement, TABLE_NAME, this::toEvent);
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't save User.", e);
        }
    }

    private Event toEvent(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        Event event = new Event();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    event.setId(resultSet.getLong(i));
                    break;
                case "userId":
                    event.setUserId(resultSet.getLong(i));
                    break;
                case "creationTime":
                    event.setCreationTime(resultSet.getTimestamp(i));
                    break;
                case "type":
                    event.setType(EventType.valueOf(resultSet.getString(i)));
                    break;
                default:
                    // No operations.
            }
        }

        return event;
    }
}
