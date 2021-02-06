package ru.ifmo.rain.mozhevitin.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;

public class ParallelMapperImpl implements ParallelMapper {
    private final Queue<Runnable> tasks;
    private final List<Thread> workers;
    private static final int MAX_QUEUE_SIZE = 1_000;

    private void doTask() throws InterruptedException {
        Runnable task;
        synchronized (tasks) {
            while (tasks.isEmpty()) {
                tasks.wait();
            }
            task = tasks.poll();
            tasks.notifyAll();
        }
        task.run();
    }

    private void addTask(final Runnable task) throws InterruptedException {
        synchronized (tasks) {
            while (tasks.size() == MAX_QUEUE_SIZE) {
                tasks.wait();
            }
            tasks.add(task);
            tasks.notifyAll();
        }
    }

    /**
     * Constructor from number of threads.
     *
     * @param threads number of threads in threadpool.
     *
     * @throws {@link IllegalArgumentException} if number of threads is non-positive.
     */
    public ParallelMapperImpl(final int threads) {
        if (threads <= 0) {
            throw new IllegalArgumentException("Number of threads must be positive.");
        }

        this.tasks = new ArrayDeque<>();
        this.workers = new ArrayList<>();

        for (int i = 0; i < threads; i++) {
            Thread worker = new Thread(() -> {
                try {
                    while (!Thread.interrupted()) {
                        doTask();
                    }
                } catch (InterruptedException ignored) {
                } finally {
                    Thread.currentThread().interrupt();
                }
            });
            workers.add(worker);
            worker.start();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> args) throws InterruptedException {
        ConcurrentListImpl<R> list = new ConcurrentListImpl<>(args.size());
        for (int i = 0; i < args.size(); i++) {
            final int index = i;
            final T arg = args.get(index);
            addTask(() -> list.setValue(index, f.apply(arg)));
        }
        return list.getValues();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void close() {
        for (Thread worker : workers) {
            worker.interrupt();
        }

        for (Thread thread : workers) {
            try {
                thread.join();
            } catch (InterruptedException ignored) {
            }
        }
    }

    private static class ConcurrentListImpl<R> {
        private final List<R> values;
        private AtomicInteger tasksDone;

        ConcurrentListImpl(final int size) {
            values = new ArrayList<>(Collections.nCopies(size, null));
            tasksDone = new AtomicInteger(0);
        }

        synchronized void notifyIfDone() {
            while (tasksDone.incrementAndGet() == values.size()) {
                notify();
            }
        }

        void setValue(final int pos, R value) {
            values.set(pos, value);
            notifyIfDone();
        }

        synchronized List<R> getValues() throws InterruptedException {
            while (tasksDone.get() < values.size()) {
                wait();
            }
            return values;
        }
    }
}