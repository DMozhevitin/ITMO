package ru.itmo.wp.model.domain;

import java.io.Serializable;
import java.util.Date;

public class Event implements Serializable, DomainEntity {
    private long id;
    private long userId;
    private EventType type;
    private Date creationTime;

    public Event() {}

    public Event(long userId, EventType type) {
        this.userId = userId;
        this.type = type;
    }

    public void setId(long id) {
        this.id = id;
    }

    public void setCreationTime(Date creationTime) {
        this.creationTime = creationTime;
    }

    public void setUserId(long userId) {
        this.userId = userId;
    }

    public void setType(EventType type) {
        this.type = type;
    }

    public long getId() {
        return id;
    }

    public long getUserId() {
        return userId;
    }

    public EventType getType() {
        return type;
    }

    public Date getCreationTime() {
        return creationTime;
    }
}

