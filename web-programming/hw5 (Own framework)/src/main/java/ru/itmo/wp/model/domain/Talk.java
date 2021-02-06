package ru.itmo.wp.model.domain;

import java.io.Serializable;
import java.util.Date;

public class Talk implements Serializable, DomainEntity {
    private long id;
    private long sourceUserId;
    private long targetUserId;
    private Date creationTime;
    private String message;

    public Talk() {}

    public Talk(long sourceUserId, long targetUserId, String message) {
        this.sourceUserId = sourceUserId;
        this.targetUserId = targetUserId;
        this.message = message;
    }

    public void setId(long id) {
        this.id = id;
    }

    public long getId() {
        return id;
    }

    public long getSourceUserId() {
        return sourceUserId;
    }

    public void setSourceUserId(long sourceUserId) {
        this.sourceUserId = sourceUserId;
    }

    public long getTargetUserId() {
        return targetUserId;
    }

    public void setTargetUserId(long targetUserId) {
        this.targetUserId = targetUserId;
    }

    public Date getCreationTime() {
        return creationTime;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public void setCreationTime(Date creationTime) {
        this.creationTime = creationTime;
    }
}
