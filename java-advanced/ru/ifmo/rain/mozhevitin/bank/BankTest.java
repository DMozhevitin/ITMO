package ru.ifmo.rain.mozhevitin.bank;

import org.junit.BeforeClass;
import org.junit.Test;

import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.Assert.*;

public class BankTest {

    private static Bank bank;
    private static Registry registry;

    private static Person personSample() throws RemoteException {
        return nthPerson(0);
    }

    private static Person nthPerson(int n) {
        Person p = null;
        try {
            p = new RemotePerson("firstName" + n,
                    "lastName" + n, "passport"
                    + n, 228 + n);
        } catch (RemoteException ignored) {
        }

        return p;
    }

    private static void registerSamplePerson() throws RemoteException {
        bank.createPerson(personSample().getFirstName(), personSample().getLastName(), personSample().getPassport());
    }

    private static void assertPersonEquals(Person a, Person b) throws RemoteException {
        assertEquals(a.getLastName(), b.getLastName());
        assertEquals(a.getFirstName(), b.getFirstName());
        assertEquals(a.getPassport(), b.getPassport());
    }

    @BeforeClass
    public static void initBank() throws RemoteException, NotBoundException {
        registry = LocateRegistry.createRegistry(Registry.REGISTRY_PORT);
        registry.rebind("//localhost/bank", new RemoteBank(8080));
        bank = (Bank) registry.lookup("//localhost/bank");

        System.out.println("Bank created");
    }

    @Test
    public void getPerson_personDoesntExist_resultIsNull() throws RemoteException {
        assertNull(bank.getLocalPerson(String.valueOf(Integer.MAX_VALUE)));
        assertNull(bank.getLocalPerson(String.valueOf(Integer.MIN_VALUE)));
    }

    @Test
    public void getPerson_personExist_resultIsNotNull() throws RemoteException {
        registerSamplePerson();
        Person remotePerson = bank.getRemotePerson(personSample().getPassport());
        assertPersonEquals(remotePerson, personSample());
    }

    @Test
    public void createLotsOfPeople() throws RemoteException {
        List<Person> people = IntStream.range(1, 128)
                .mapToObj(BankTest::nthPerson)
                .collect(Collectors.toList());

        for (Person p : people) {
            bank.createPerson(p.getFirstName(), p.getLastName(), p.getPassport());
            assertPersonEquals(p, bank.getRemotePerson(p.getPassport()));
        }
    }

    @Test
    public void getAccounts_newPerson_hasNoAccounts() throws RemoteException {
        registerSamplePerson();
        assertTrue(bank.getPersonAccounts(personSample()).isEmpty());
    }

    @Test
    public void createAndGetAccounts_AccountsAreCreated() throws RemoteException {
        registerSamplePerson();
        for (int i = 1; i <= 64; i++) {
            bank.createAccount(personSample(), String.valueOf(i));
        }

        assertEquals(64, bank.getPersonAccounts(personSample()).size());
    }
}