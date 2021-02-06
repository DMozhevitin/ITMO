package ru.itmo.wp.form.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itmo.wp.form.RegisterForm;
import ru.itmo.wp.form.UserCredentials;
import ru.itmo.wp.service.UserService;

@Component
public class RegisterFormValidator implements Validator {
    private final UserService userService;

    public RegisterFormValidator(UserService userService) {
        this.userService = userService;
    }

    @Override
    public boolean supports(Class<?> aClass) {
        return RegisterForm.class.equals(aClass);
    }

    @Override
    public void validate(Object target, Errors errors) {
        if (!errors.hasErrors()) {
            RegisterForm registerForm = (RegisterForm) target;
            if (userService.findByLoginAndPassword(
                    registerForm.getLogin(), registerForm.getPassword()) != null) {
                errors.reject("login.invalid-login", "This user is already registered");
            }
        }
    }
}
