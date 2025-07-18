/*
 * MIT License
 *
 * Copyright (c) 2019-2025 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package com.github.weisj.darklaf.nativeutil;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.*;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;


/**
 * A simple library class which helps with loading dynamic libraries stored in the JAR archive.
 * These libraries usually contain implementation of some methods in native code (using JNI - Java
 * Native Interface).
 *
 * @see <a href=
 *      "http://adamheinrich.com/blog/2012/how-to-load-native-jni-library-from-jar">http://adamheinrich.com/blog/2012/how-to-load-native-jni-library-from-jar</a>
 * @see <a href=
 *      "https://github.com/adamheinrich/native-utils">https://github.com/adamheinrich/native-utils</a>
 */
public final class NativeUtil {

    private static final Logger LOGGER = Logger.getLogger(NativeUtil.class.getName());
    public static final String NATIVE_FOLDER_PATH_PREFIX = "com-weisj-darklaf-nativeutils";
    /**
     * The minimum length a prefix for a file has to have according to
     * {@link File#createTempFile(String, String)}}.
     */
    private static final int MIN_PREFIX_LENGTH = 3;
    /** Temporary directory which will contain the DLLs. */
    private static Path temporaryDir;

    private NativeUtil() {}

    public record Resource(String filePath, String destinationDirectoryPath) {
    }

    /**
     * Loads library from current JAR archive
     *
     * <p>
     * The file from JAR is copied into system temporary directory and then loaded. The temporary file
     * is deleted after exiting. Method uses String as filename because the pathname is "abstract", not
     * system-dependent.
     *
     * @param loaderClass the class to use for loading.
     * @param path The path of file inside JAR as absolute path (beginning with '/'), e.g.
     *        /package/File.ext
     * @param identifier The library identifier for clean-up purposes
     * @throws IOException If temporary file creation or read/write operation fails
     * @throws IllegalArgumentException If source file (param path) does not exist
     * @throws IllegalArgumentException If the path is not absolute or if the filename is shorter than
     *         three characters (restriction of
     *         {@link File#createTempFile(java.lang.String, java.lang.String)}).
     * @throws FileNotFoundException If the file could not be found inside the JAR.
     */
    public static void loadLibraryFromJarWithExtraResources(final Class<?> loaderClass, final String path,
            final List<Resource> resources, final String identifier)
            throws IOException {

        String libraryIdentifier = getFullLibraryIdentifier(identifier);
        Path tempDir = getTemporaryDirectory(libraryIdentifier);

        List<Path> resourcePaths = extractResources(loaderClass, resources, tempDir);
        try {
            doLoadLibraryFromJar(loaderClass, path, identifier, tempDir);
        } finally {
            resourcePaths.forEach(NativeUtil::releaseResource);
        }
    }

    private static List<Path> extractResources(final Class<?> caller, final List<Resource> resources,
            final Path tempDir)
            throws IOException {
        List<Path> paths = new ArrayList<>(resources.size());

        for (Resource resource : resources) {
            String filename = getFileNameFromPath(resource.filePath);
            Path destinationDir = tempDir.resolve(resource.destinationDirectoryPath);
            Path destinationPath = destinationDir.resolve(Objects.requireNonNull(filename));
            try {
                Files.createDirectories(destinationDir);
                extractFile(caller, resource.filePath, tempDir, destinationPath);
            } catch (final IOException e) {
                paths.forEach(NativeUtil::delete);
                throw e;
            }
            paths.add(tempDir.resolve(Objects.requireNonNull(filename)));
        }
        return paths;
    }

    /**
     * Loads library from current JAR archive
     *
     * <p>
     * The file from JAR is copied into system temporary directory and then loaded. The temporary file
     * is deleted after exiting. Method uses String as filename because the pathname is "abstract", not
     * system-dependent.
     *
     * @param loaderClass the class to use for loading.
     * @param path The path of file inside JAR as absolute path (beginning with '/'), e.g.
     *        /package/File.ext
     * @param identifier The library identifier for clean-up purposes
     * @throws IOException If temporary file creation or read/write operation fails
     * @throws IllegalArgumentException If source file (param path) does not exist
     * @throws IllegalArgumentException If the path is not absolute or if the filename is shorter than
     *         three characters (restriction of
     *         {@link File#createTempFile(java.lang.String, java.lang.String)}).
     * @throws FileNotFoundException If the file could not be found inside the JAR.
     */
    public static void loadLibraryFromJar(final Class<?> loaderClass, final String path, final String identifier)
            throws IOException {
        String libraryIdentifier = getFullLibraryIdentifier(identifier);
        Path tempDir = getTemporaryDirectory(libraryIdentifier);
        doLoadLibraryFromJar(loaderClass, path, identifier, tempDir);
    }

    private static void doLoadLibraryFromJar(final Class<?> loaderClass, final String path, final String identifier,
            final Path tempDir) throws IOException {
        String filename = getFileNameFromPath(path);

        // Check if the filename is okay
        if (filename == null || filename.length() < MIN_PREFIX_LENGTH) {
            throw new IllegalArgumentException("The filename has to be at least 3 characters long.");
        }

        // Prepare temporary file
        String libraryIdentifier = getFullLibraryIdentifier(identifier);
        Path temp = tempDir.resolve(filename);

        if (!isPosixCompliant()) {
            deleteLeftoverTempFiles(tempDir, libraryIdentifier);
        }
        extractFile(loaderClass, path, tempDir, temp);

        try {
            System.load(temp.toAbsolutePath().toString());
        } finally {
            releaseResource(temp);
        }
    }

    private static void extractFile(final Class<?> loaderClass, final String path, final Path destinationDir,
            final Path destinationPath)
            throws IOException {
        try (InputStream is = loaderClass.getResourceAsStream(path)) {
            if (is == null) throw new FileNotFoundException("File " + path + " was not found inside JAR.");
            if (!destinationDir.toFile().canWrite()) throw new IOException("Can't write to temporary directory.");
            if (!Files.exists(destinationPath)) {
                // Otherwise, the file is already existent and most probably loaded.
                Files.copy(is, destinationPath.toAbsolutePath(), StandardCopyOption.REPLACE_EXISTING);
            }
        } catch (final IOException e) {
            delete(destinationPath);
            throw e;
        }
    }

    private static void deleteLeftoverTempFiles(Path tempDir, String identifier) throws IOException {
        try (Stream<Path> files = Files.list(tempDir.getParent())) {
            files.filter(Files::isDirectory)
                    .filter(p -> !tempDir.equals(p))
                    .filter(p -> p.getFileName().toString().startsWith(identifier))
                    .forEach(NativeUtil::deleteFolder);
        }
    }

    /**
     * Recursively deletes a folder and it's files
     *
     * @param folder the target folder as {@link File}
     */
    private static void deleteFolder(Path folder) {
        LOGGER.fine("Removing " + folder);
        try (Stream<Path> walk = Files.walk(folder)) {
            walk.sorted(Comparator.reverseOrder())
                    .forEach(NativeUtil::delete);
        } catch (IOException e) {
            LOGGER.log(Level.WARNING, "Could not delete directory", e);
        }
    }

    private static String getFileNameFromPath(final String path) {
        checkPath(path);
        String[] parts = path.split("/");
        return (parts.length > 1) ? parts[parts.length - 1] : null;
    }

    private static void checkPath(final String path) {
        if (null == path || !path.startsWith("/")) {
            throw new IllegalArgumentException("The path has to be absolute (start with '/').");
        }
    }

    private static String getFullLibraryIdentifier(final String identifier) {
        return NATIVE_FOLDER_PATH_PREFIX + "-" + identifier;
    }

    private static Path getTemporaryDirectory(final String libraryIdentifier) throws IOException {
        if (temporaryDir == null) {
            temporaryDir = createTempDirectory(libraryIdentifier);
            temporaryDir.toFile().deleteOnExit();
        }
        return temporaryDir;
    }

    private static void releaseResource(final Path path) {
        if (isPosixCompliant()) {
            // Assume POSIX compliant file system, can be deleted after loading
            delete(path);
        } else {
            // Assume non-POSIX, and don't delete until last file descriptor closed
            path.toFile().deleteOnExit();
        }
    }

    private static void delete(final Path path) {
        try {
            Files.deleteIfExists(path);
        } catch (final IOException e) {
            LOGGER.log(Level.WARNING, "Couldn't delete file " + path, e);
        }
    }

    private static Path createTempDirectory(final String prefix) throws IOException {
        return Files.createTempDirectory(prefix + System.nanoTime());
    }

    private static boolean isPosixCompliant() {
        try {
            return FileSystems.getDefault().supportedFileAttributeViews().contains("posix");
        } catch (FileSystemNotFoundException | ProviderNotFoundException | SecurityException e) {
            return false;
        }
    }
}
