package ru.ifmo.rain.mozhevitin.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Phaser;

import static java.util.concurrent.Executors.newFixedThreadPool;

public class WebCrawler implements Crawler {
    private final Downloader downloader;
    private final int perHost;
    private final ExecutorService extractorsPool;
    private final ExecutorService downloadersPool;
    private final ConcurrentMap<String, OnHostWorker> name2Host;

    /**
     * Creates instance of {@link WebCrawler} with specific params.
     *
     * @param downloader downloader that allows to download pages and extract links from them.
     * @param downloaders maximum number of page downloaders threads.
     * @param extractors maximum number of extractors threads.
     * @param perHost maximum number of threads that can download pages from specific host.
     */
    public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost) {
        this.downloader = downloader;
        this.perHost = perHost;
        this.name2Host = new ConcurrentHashMap<>();

        this.extractorsPool = newFixedThreadPool(extractors);
        this.downloadersPool = newFixedThreadPool(downloaders);
    }

    /**
     * Creates instance of {@link WebCrawler} with specific {@link RunConfiguration}.
     *
     * @param config specific {@link RunConfiguration} for this instance of {@link WebCrawler}
     */
    public WebCrawler(RunConfiguration config) {
        this(config.getDownloader(), config.getDownloads(), config.getExtractors(), config.getPerHost());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Result download(final String url, final int depth) {
        final Phaser phaser = new Phaser(1);
        final Set<String> downloaded = ConcurrentHashMap.newKeySet();
        final Set<String> visited = ConcurrentHashMap.newKeySet();
        final Map<String, IOException> failed = new ConcurrentHashMap<>();

        visited.add(url);

        download(url, depth, downloaded, failed, visited, phaser);
        phaser.arriveAndAwaitAdvance();

        return new Result(new ArrayList<>(downloaded), failed);
    }

    /**
     * Download page and all of its inner links if {@param depth} > 1 in parallel mode.
     *
     * @param url url of the loaded page
     * @param depth of inner pages
     * @param downloaded set of successfully downloaded urls.
     * @param failed {@link Map} from failed url to list of errors represented by {@link IOException}
     * @param visited set of already visited links
     * @param phaser phaser which provides synchronization of downloaders and link extractors threads.
     */
    private void download(final String url, final int depth, final Set<String> downloaded,
                          final Map<String, IOException> failed, final Set<String> visited, final Phaser phaser) {
        String hostName;
        try {
            hostName = URLUtils.getHost(url);
        } catch (MalformedURLException e) {
            failed.put(url, e);
            return;
        }

        final OnHostWorker worker = name2Host.computeIfAbsent(hostName, name -> new OnHostWorker(perHost, name));

        phaser.register();
        worker.submitTask(() -> {
            try {
                final Document document = downloader.download(url);
                downloaded.add(url);

                if (depth > 1) {
                    phaser.register();
                    extractorsPool.submit(() -> {
                        try {
                            final List<String> links = document.extractLinks();
                            for (String link : links) {
                                if (!visited.contains(link)) {
                                    visited.add(link);
                                    download(link, depth - 1, downloaded, failed, visited, phaser);
                                }
                            }
                        } catch (IOException ignored) {
                        } finally {
                            phaser.arrive();
                        }
                    });
                }
            } catch (IOException e) {
                failed.put(url, e);
            }
            finally {
                phaser.arrive();
                worker.executeWaitingTask();
            }
        });

    }


    /**
     * {@inheritDoc}
     */
    @Override
    public void close() {
        extractorsPool.shutdownNow();
        downloadersPool.shutdownNow();
    }


    /**
     * Provides command line interface to run {@link WebCrawler#download(String, int)} with specific crawler
     * configuration presented by {@link RunConfiguration}.
     *
     * @param args command line arguments to set-up {@link RunConfiguration}. It can be one of following cases.
     * <li>
     *    <ul> url depth </ul>
     *    <ul> url depth downloads </ul>
     *    <ul> url depth downloads extractors </ul>
     *    <ul> url depth downloads extractors perHost</ul>
     * </li>
     *
     * To find out the meaning of the parameters check
     * {@link WebCrawler#WebCrawler(Downloader, int, int, int)} and
     * {@link WebCrawler#download(String, int)}
     */
    public static void main(String[] args) {
        if (args == null || args.length < 2 || args.length > 5) {
            System.err.println("Incorrect arguments count. From 2 to 5 arguments expected.");
            return;
        }

        for (final String arg : args) {
            if (arg == null) {
                System.err.println("All args must be non-null");
                break;
            }
        }

        String url = args[0];
        Integer downloaders, extractors, perHost;
        try {
            int depth = Integer.parseInt(args[1]);
            downloaders = args.length > 2 ? Integer.parseInt(args[2]) : null;
            extractors = args.length > 3 ? Integer.parseInt(args[3]) : null;
            perHost = args.length > 4 ? Integer.parseInt(args[4]) : null;

            try (Crawler crawler = new WebCrawler(new RunConfiguration(new CachingDownloader(),
                    downloaders, extractors, perHost))) {
                crawler.download(url, depth);
            } catch (IOException e) {
                System.err.println("Unable to create Downloader: " + e.getMessage());
            }

        } catch (NumberFormatException e) {
            System.err.println("Cannot parse argument. Only integer values expected.");
        }
    }

    /**
     * Inner class that controls the downloads attached to the specific host and
     * compliance to restriction <code>downloads < perHost</code>.
     */
    private class OnHostWorker {
        final Queue<Runnable> waitingTasks;
        private int inWork;
        private final int perHost;
        private final String hostName;

        OnHostWorker(int perHost, String hostName) {
            this.waitingTasks = new ArrayDeque<>();
            inWork = 0;
            this.perHost = perHost;
            this.hostName = hostName;
        }

        private synchronized void submitTask(Runnable task) {
            if (inWork >= perHost) {
                waitingTasks.add(task);
            } else {
                inWork++;
                downloadersPool.submit(task);
            } 
        }

        private synchronized void executeWaitingTask() {
            if (waitingTasks.peek() != null) {
                downloadersPool.submit(waitingTasks.poll());
            } else {
                inWork--;
            }
        }
    }

    /**
     * Inner class that defines params with which {@link WebCrawler#download(String, int)} will be launched.
     */
    static class RunConfiguration {
        private static int DEFAULT_PARAM_VALUE = 8;

        private final Downloader downloader;

        private int downloads;

        private int extractors;

        private int perHost;

        int getDownloads() {
            return downloads;
        }

        int getExtractors() {
            return extractors;
        }

        int getPerHost() {
            return perHost;
        }

        Downloader getDownloader() {
            return downloader;
        }


        RunConfiguration(Downloader downloader, Integer downloads, Integer extractors, Integer perHost) {
            this.downloader = downloader;
            this.downloads = Optional.ofNullable(downloads).orElse(DEFAULT_PARAM_VALUE);
            this.extractors = Optional.ofNullable(extractors).orElse(DEFAULT_PARAM_VALUE);
            this.perHost = Optional.ofNullable(perHost).orElse(DEFAULT_PARAM_VALUE);
        }
    }
}
