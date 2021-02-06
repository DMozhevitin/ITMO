package ru.ifmo.rain.mozhevitin.bank;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;

public class Client {
    public static void main(String[] args) throws RemoteException {
        try {
            Bank bank;
            try {
                bank = (Bank)Naming.lookup("//localhost/bank");
            } catch (final NotBoundException e) {
                System.out.println("Object isn't bound");
                return;
            } catch (final MalformedURLException e) {
                System.out.println("Malformed url.");
                return;
            }

            if (args.length != 5) {
                System.err.println("Expected 5 arguments.");
                return;
            }

            int change;
            String firstName = args[0];
            String lastName = args[1];
            String passport = args[2];
            String accountId = args[3];

            try {
                change = Integer.parseInt(args[4]);
            } catch (NumberFormatException nfe) {
                System.err.println("Last argument should be integer.");
                return;
            }

            Person person = bank.getRemotePerson(passport);
            if (person == null) {
                bank.createPerson(firstName, lastName, passport);
                person = bank.getRemotePerson(passport);
                System.out.println("New person has been created.");
            }

            if (!bank.getPersonAccounts(person).contains(accountId)) {
                Account account = bank.getAccount(person, accountId);
                System.out.println("New account has been created.");
                if (account != null) {
                    System.err.println("This account belongs to another person.");
                    return;
                }
                bank.createAccount(person, accountId);
            }

            Account account = bank.getAccount(person, accountId);
            account.setAmount(account.getAmount() + change);
            System.out.println("Current balance: " + account.getAmount() + " on account " + account.getId());

        } catch (RemoteException e) {
            System.out.println("An error occurred while working with remote bank: " + e.getMessage());
        }
    }
}
