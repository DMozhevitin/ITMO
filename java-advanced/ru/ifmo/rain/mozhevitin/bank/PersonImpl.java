package ru.ifmo.rain.mozhevitin.bank;


public class PersonImpl implements Person {
    private String firstName;
    private String lastName;
    private String passport;

    PersonImpl(String firstName, String lastName, String passport) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.passport = passport;
    }

    @Override
    public String getFirstName() {
        return firstName;
    }

    @Override
    public String getLastName() {
        return lastName;
    }

    @Override
    public String getPassport() {
        return passport;
    }
}
