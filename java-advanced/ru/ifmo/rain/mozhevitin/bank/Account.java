package ru.ifmo.rain.mozhevitin.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Account extends Remote {
    String getId() throws RemoteException;

    int getAmount() throws RemoteException;

    void setAmount(int amount) throws RemoteException;

    default void incrementAmount(int cntMoney) throws RemoteException {
        setAmount(getAmount() + cntMoney);
    }

    default void withdraw(int cntMoney) throws RemoteException {
        setAmount(getAmount() - cntMoney);
    }
}