package ru.ifmo.rain.mozhevitin.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.stream.Stream;

public class RemoteBank extends UnicastRemoteObject implements Bank {
    private int port;
    private ConcurrentMap<String, Person> id2Person;
    private ConcurrentMap<String, Account> id2Account;
    private ConcurrentMap<String, Set<String>> passport2Accounts;

    public RemoteBank(int port) throws RemoteException {
        super(port);
        this.port = port;
        this.id2Person = new ConcurrentHashMap<>();
        this.id2Account = new ConcurrentHashMap<>();
        this.passport2Accounts = new ConcurrentHashMap<>();
    }

    @Override
    public Person getLocalPerson(String passport) throws RemoteException {
        Person person = Optional.ofNullable(passport)
                .map(id2Person::get)
                .orElse(null);

        if (anyNull(person)) {
            return null;
        }

        Map<String, LocalAccount> personAccounts = new ConcurrentHashMap<>();
        for (String name : getPersonAccounts(person)) {
            personAccounts.put(name, new LocalAccount(getAccount(person, name).getId(), getAccount(person, name).getAmount()));
        }
        return new LocalPerson(person.getFirstName(),
                person.getLastName(),
                person.getPassport(),
                personAccounts);
    }

    @Override
    public boolean checkPerson(String firstName, String lastName, String passport) throws RemoteException {
        if (anyNull(firstName, lastName, passport)) {
            return false;
        }
        Person person = id2Person.get(passport);
        return Objects.nonNull(person) && Objects.equals(person.getFirstName(), firstName) && Objects.equals(person.getLastName(), lastName);
    }

    @Override
    public Person getRemotePerson(String passport) throws RemoteException {
        return Optional.ofNullable(passport)
                .map(id2Person::get)
                .orElse(null);
    }

    @Override
    public Set<String> getPersonAccounts(Person person) throws RemoteException {
        if (anyNull(person)) {
            return null;
        }

        if (person instanceof LocalPerson)
            return ((LocalPerson) person).getAccounts();

        return passport2Accounts.get(person.getPassport());
    }

    @Override
    public boolean createPerson(String firstName, String lastName, String passport) throws RemoteException {
        if (anyNull(firstName, lastName, passport) || id2Person.containsKey(passport)) {
            return false;
        }

        id2Person.put(passport, new RemotePerson(firstName, lastName, passport, port));
        passport2Accounts.put(passport, ConcurrentHashMap.newKeySet());
        return true;
    }

    public Account getAccount(Person person, String id) throws RemoteException {
        if (anyNull(person, id)) {
            return null;
        }

        Account account = id2Account.get(toAccountId(person, id));

        if (account == null)
            return null;

        if (person instanceof LocalPerson) {
            return ((LocalPerson) person).getAccount(id);
        }
        return account;
    }

    @Override
    public boolean createAccount(Person person, String id) throws RemoteException {
        if (anyNull(person, id)) {
            return false;
        }

        String accountId = toAccountId(person, id);
        if (id2Account.containsKey(accountId)) {
            return false;
        }

        final Account account = new RemoteAccount(id, port);

        id2Account.put(accountId, account);
        if (passport2Accounts.get(person.getPassport()) == null)
            passport2Accounts.put(person.getPassport(), new ConcurrentSkipListSet<>());
        passport2Accounts.get(person.getPassport()).add(id);

        return true;
    }

    private String toAccountId(Person person, String id) throws RemoteException {
        return String.format("%s:%s", person.getPassport(), id);
    }

    private boolean anyNull(Object... args) {
        return Stream.of(args)
                .anyMatch(Objects::isNull);
    }
}
