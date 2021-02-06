package ru.itmo.wp.form.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itmo.wp.domain.Tag;
import ru.itmo.wp.form.PostForm;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class PostFormValidator implements Validator {
    @Override
    public boolean supports(Class<?> aClass) {
        return PostForm.class.equals(aClass);
    }

    @Override
    public void validate(Object target, Errors errors) {
        if (!errors.hasErrors()) {
            PostForm postForm = (PostForm) target;

            List<Tag> tags = Arrays.stream(
                    postForm.getTags().split("\\s+"))
                    .map(Tag::new)
                    .collect(Collectors.toList());

            List<Tag> invalidTags = tags.stream()
                                    .filter(t -> (!t.getName().matches("[a-zA-Z]*") || t.getName().length() > 16))
                                    .collect(Collectors.toList());

            if (!invalidTags.isEmpty()) {
                errors.rejectValue("tags", "tags.invalid-tags", "wrong tag: " + invalidTags.get(0).getName());
            }
        }
    }
}
