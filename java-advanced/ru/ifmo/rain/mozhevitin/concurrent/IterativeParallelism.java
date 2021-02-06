package ru.ifmo.rain.mozhevitin.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ListIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class IterativeParallelism implements ListIP {
    private ParallelMapper parallelMapper = null;

    public IterativeParallelism(ParallelMapper parallelMapper) {
        this.parallelMapper = parallelMapper;
    }

    public IterativeParallelism() {}

    @Override
    public String join(int threads, List<?> values) throws InterruptedException {
        return commonParallelTask(threads, values,
                stream -> stream
                        .map(Object::toString)
                        .collect(Collectors.joining()),
                stream -> stream
                        .collect(Collectors.joining())
        );
    }

    @Override
    public <T> List<T> filter(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return commonParallelTask(threads, values,
                stream -> stream.
                        filter(predicate)
                        .collect(Collectors.toList()),
                stream -> stream
                        .flatMap(Collection::stream)
                        .collect(Collectors.toList())
        );
    }

    @Override
    public <T, U> List<U> map(int threads, List<? extends T> values, Function<? super T, ? extends U> f) throws InterruptedException {
        return commonParallelTask(threads, values,
                stream -> stream.map(f)
                        .collect(Collectors.toList()),
                stream -> stream
                        .flatMap(Collection::stream)
                        .collect(Collectors.toList())
        );
    }

    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        if (values.isEmpty()) {
            throw new IllegalArgumentException("Values must be non-empty.");
        }

        Function<Stream<? extends T>, ? extends T> max = stream -> stream.max(comparator).get();
        return commonParallelTask(threads, values, max, max);
    }

    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        if (values.isEmpty()) {
            throw new IllegalArgumentException("Values must be non-empty.");
        }

        Function<Stream<? extends T>, ? extends T> min = stream -> stream.min(comparator).get();
        return commonParallelTask(threads, values, min, min);
    }

    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return commonParallelTask(threads, values,
                stream -> stream.allMatch(predicate),
                stream -> stream.allMatch(Boolean::booleanValue));
    }

    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return commonParallelTask(threads, values,
                stream -> stream.anyMatch(predicate),
                stream -> stream.anyMatch(Boolean::booleanValue));
    }

    private <T, R> R commonParallelTask(int threads, List<? extends T> values,
                                        Function<Stream<? extends T>, ? extends R> function,
                                        Function<Stream<? extends R>, ? extends R> collector) throws InterruptedException {
        if (threads <= 0) {
            throw new IllegalArgumentException("Number of threads must be positive.");
        }

        List<Stream<? extends T>> subtasks = new ArrayList<>();
        List<R> res;
        threads = Math.min(threads, values.size());
        threads = Math.max(threads, 1);

        int sizeOfBlock = values.size() / threads;
        int rest = values.size() % threads;

        int left = 0, right = 0;
        for (int i = 0; i < threads; i++) {
            left = right;
            right = left + sizeOfBlock;
            if (rest > 0) {
                right++;
                rest--;
            }

            subtasks.add(values.subList(left, right).stream());
        }

        if (parallelMapper != null) {
            res = parallelMapper.map(function, subtasks);
        } else {
            List<Thread> workers = new ArrayList<>();
            res = new ArrayList<>(Collections.nCopies(threads, null));

            for (int i = 0; i < threads; i++) {
                int pos = i;
                Thread worker = new Thread(() -> res.set(pos, function.apply(subtasks.get(pos))));
                workers.add(worker);
                worker.start();
            }

            for (Thread thread : workers) {
                try {
                    thread.join();
                } catch (InterruptedException ignored) {
                }
            }
        }

        return collector.apply(res.stream());
    }
}