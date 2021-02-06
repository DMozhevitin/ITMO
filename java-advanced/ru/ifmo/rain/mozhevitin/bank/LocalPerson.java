package ru.ifmo.rain.mozhevitin.bank;

import java.io.Serializable;
import java.util.Map;
import java.util.Set;

public class LocalPerson extends PersonImpl implements Person, Serializable {
    private Map<String, LocalAccount> accounts;

    LocalPerson(String firstName, String lastName, String pasportId, Map<String, LocalAccount> accounts) {
        super(firstName, lastName, pasportId);
        this.accounts = accounts;
    }

    Set<String> getAccounts() {
        return accounts.keySet();
    }

    Account getAccount(String accountId) {
        return accounts.get(accountId);
    }
}
