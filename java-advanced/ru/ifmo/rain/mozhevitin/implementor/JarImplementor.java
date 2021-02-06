package ru.ifmo.rain.mozhevitin.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.zip.ZipEntry;

public class JarImplementor extends Implementor implements JarImpler {
    /**
     * Compiles class, which implements given {@code token} class.
     *
     * @param token type token to create implementation for
     * @param path  directory to store compiled file
     * @throws ImplerException if compilation fails for some reason
     */
    private void compile(final Class<?> token, final Path path) throws ImplerException {
        final String file = Path.of(path.toString(),
                token.getPackageName().replace('.',
                        File.separatorChar), CodeGenUtils.getClassName(token) + ".java").toString();
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            throw new ImplerException("Could not find java compiler");
        }

        final List<String> args = new ArrayList<>();
        args.add(file);
        args.add("-cp");
        args.add(path + File.pathSeparator + getClassPath(token));

        final int exitCode = compiler.run(null, null, null, args.toArray(String[]::new));
        if (exitCode != 0) {
            throw new ImplerException("An error occurred while compiling implementation");
        }
    }

    /**
     * Main function which implements console interface for {@link Implementor}.
     * Pass 2 args to create source code file.
     * Pass 3 args to create jar file
     *
     * @param args console arguments
     */
    public static void main(final String[] args) {
        if (validateArgs(args)) {
            return;
        }

        if (args == null || (args.length != 2 && args.length != 3)) {
            System.err.println("2 or 3 args expected.");
            return;
        }

        if (args.length == 3 && !args[0].equals("-jar") && !args[0].equals("--jar")) {
            System.err.println(String.format("Expected -jar, found %s", args[0]));
            return;
        }

        final Implementor implementor = new Implementor();
        try {
            if (args.length == 2) {
                implementor.implement(Class.forName(args[0]), Path.of(args[1]));
            }
        } catch (final ClassNotFoundException e) {
            System.err.println("An error occurred while implementing class given: " + e.getMessage());
        } catch (final InvalidPathException e) {
            System.err.println("An error occurred while creating path for output file: " + e.getMessage());
        } catch (final ImplerException e) {
            System.err.println("An error occurred while generating implementation code for class given: " + e.getMessage());
        }
    }

    /**
     * Returns file path string representation
     * @param token class to get path
     * @return file path
     */
    private static String getClassPath(Class<?> token) {
        try {
            return Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (final URISyntaxException e) {
            throw new AssertionError(e);
        }
    }

    /**
     * Creates {@code .jar} file containing compiled by {@link #compile(Class, Path)}
     * implementation of {@code token}
     *
     * @param token   class that was implemented
     * @param jarPath directory to store {@code .jar} file
     * @param tmpPath directory containing compiled implementation of {@code token}
     * @throws ImplerException if error occurs during {@link JarOutputStream} work
     */
    private void generateArtifact(final Class<?> token, final Path jarPath, final Path tmpPath) throws ImplerException {
        final Manifest manifest = new Manifest();
        manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");

        try (final JarOutputStream jos = new JarOutputStream(Files.newOutputStream(jarPath), manifest)) {
            final String name = token.getPackageName().replace('.', '/') + "/" + CodeGenUtils.getClassName(token) + ".class";
            jos.putNextEntry(new ZipEntry(name));
            Files.copy(Paths.get(tmpPath.toString(), name), jos);
        } catch (final IOException e) {
            throw new ImplerException(e.getMessage());
        }
    }

    /**
     * Implements {@code token} class and creates jar-file.
     *
     * @param token   type token to create implementation for.
     * @param jarFile target {@code .jar} file.
     * @throws ImplerException if implementation fails for some reason
     */
    @Override
    public void implementJar(final Class<?> token, final Path jarFile) throws ImplerException {
        final Path tmp;
        try {
            createPath(jarFile);
            tmp = Files.createTempDirectory(jarFile.toAbsolutePath().getParent(), "tmp");
        } catch (final IOException e) {
            throw new ImplerException(e.getMessage());
        }

        try {
            implement(token, tmp);
            compile(token, tmp);
            generateArtifact(token, jarFile, tmp);
        } finally {
            try {
                Files.walkFileTree(tmp, new DirectoryCleanFileVisitor());
            } catch (final IOException e) {
                System.err.println("Failed to delete temporary directory");
            }
        }
    }


    /**
     * Class to recursively directory deletions. Extends {@link SimpleFileVisitor}
     */
    private static class DirectoryCleanFileVisitor extends SimpleFileVisitor<Path> {

        /**
         * Default constructor to create new instance of {@link DirectoryCleanFileVisitor}. Uses super constructor
         */
        DirectoryCleanFileVisitor() {
            super();
        }

        /**
         * File visitor, which deletes visited files
         *
         * @param file  {@link Path} file to visit
         * @param attrs {@link BasicFileAttributes} {@code file} attributes
         * @return {@link FileVisitResult#CONTINUE}
         * @throws IOException if deletion fails for some reason
         */
        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        /**
         * Directory visitor, which deletes directory after visiting all files in it
         *
         * @param dir {@link Path} directory to visit
         * @param exc {@link IOException} instance if error occured during directory visiting
         * @return {@link FileVisitResult#CONTINUE}
         * @throws IOException if deletion fails for some reason
         */
        @Override
        public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }
    }
}
