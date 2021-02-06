package ru.ifmo.rain.mozhevitin.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class RemotePerson extends PersonImpl implements Person {
    RemotePerson(String firstName, String lastName, String passport, int port) throws RemoteException {
        super(firstName, lastName, passport);
        UnicastRemoteObject.exportObject(this, port);
    }
}
