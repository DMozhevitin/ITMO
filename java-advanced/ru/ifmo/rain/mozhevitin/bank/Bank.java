package ru.ifmo.rain.mozhevitin.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;
import java.util.Set;

public interface Bank extends Remote {

    boolean createPerson(String name, String surname, String passport) throws RemoteException;

    boolean checkPerson(String name, String surname, String passport) throws RemoteException;

    Person getLocalPerson(String passport) throws RemoteException;

    Person getRemotePerson(String passport) throws RemoteException;

    Set<String> getPersonAccounts(Person person) throws RemoteException;

    boolean createAccount(Person person, String id) throws RemoteException;

    Account getAccount(Person person, String id) throws RemoteException;
}