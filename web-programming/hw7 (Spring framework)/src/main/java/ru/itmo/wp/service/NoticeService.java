package ru.itmo.wp.service;

import org.springframework.stereotype.Service;
import ru.itmo.wp.domain.Notice;
import ru.itmo.wp.form.NoticeForm;
import ru.itmo.wp.repository.NoticeRepository;

import java.util.List;

@Service
public class NoticeService {
    private final NoticeRepository noticeRepository;

    public NoticeService(NoticeRepository noticeRepository) {
        this.noticeRepository = noticeRepository;
    }

    public List<Notice> findAllNotices() {
        return noticeRepository.findAll();
    }

    public void addNotice(NoticeForm noticeForm) {
        Notice notice = new Notice();
        notice.setContent(noticeForm.getNoticeContent());
        noticeRepository.save(notice);
    }
}
