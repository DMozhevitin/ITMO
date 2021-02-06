package ru.itmo.wp.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import ru.itmo.wp.domain.Comment;

public interface CommentRepository extends JpaRepository<Comment, Long> {
}
