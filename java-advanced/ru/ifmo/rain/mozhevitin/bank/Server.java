package ru.ifmo.rain.mozhevitin.bank;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.RemoteException;

public class Server {
    public static void main(String[] args) throws RemoteException {
        try {
            final Bank bank = new RemoteBank(8080);
            Naming.rebind("//localhost/bank", bank);
        } catch (final RemoteException e) {
            System.out.println("Unable to export bank: " + e.getMessage());
            return;
        } catch (final MalformedURLException e) {
            System.out.println("Malformed URL");
            return;
        }
        System.out.println("Server is up.");
    }
}
