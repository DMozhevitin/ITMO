package ru.itmo.wp.model.domain;

import java.util.Date;

public interface DomainEntity {
    void setId(long id);
    void setCreationTime(Date creationTime);
    long getId();
    Date getCreationTime();
}
