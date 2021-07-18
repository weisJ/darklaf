/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
 *
 */
package com.github.weisj.darklaf.core.test;

import com.github.weisj.darklaf.nativeutil.NativeUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;

import com.github.weisj.darklaf.platform.macos.MacOSLibrary;
import com.github.weisj.darklaf.platform.windows.WindowsLibrary;

import java.util.List;

/**
 * Note: These test aren't located in their respective subprojects to ensure they are run using the
 * packaged jar and not the resource path on disc.
 */
class NativeLibraryTest {

    @Test
    void testLibrariesArePackages() {
        TestWindowsLibrary windowsLibrary = new TestWindowsLibrary();
        Assertions.assertNotNull(WindowsLibrary.class.getResource(
                windowsLibrary.getX64Path() + windowsLibrary.getLibraryName()),
                "x64 library doesn't exist");
        Assertions.assertNotNull(WindowsLibrary.class.getResource(
                windowsLibrary.getX86Path() + windowsLibrary.getLibraryName()),
                "x86 library doesn't exist");
        checkResourcesExists(WindowsLibrary.class, windowsLibrary.getResourcePaths());

        TestMacOsLibrary macOSLibrary = new TestMacOsLibrary();
        Assertions.assertNotNull(MacOSLibrary.class.getResource(
                macOSLibrary.getX64Path() + macOSLibrary.getLibraryName()),
                "x86-64 macOS library doesn't exist");
        Assertions.assertNotNull(MacOSLibrary.class.getResource(
                macOSLibrary.getArm64Path() + macOSLibrary.getLibraryName()),
                "arm64 macOS library doesn't exist");
        checkResourcesExists(MacOSLibrary.class, macOSLibrary.getResourcePaths());
    }

    private void checkResourcesExists(final Class<?> lookupClass, final List<NativeUtil.Resource> resources) {
        for (NativeUtil.Resource resource : resources) {
            Assertions.assertNotNull(lookupClass.getResource(resource.filePath));
        }
    }

    @Test
    @EnabledOnOs(OS.MAC)
    void testMacOSLibraryLoading() {
        MacOSLibrary library = new TestMacOsLibrary();
        Assertions.assertDoesNotThrow(library::updateLibrary);
        Assertions.assertTrue(library.isLoaded(), "MacOS library isn't loaded");
    }

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void testWindowsLibraryLoading() {
        WindowsLibrary library = new TestWindowsLibrary();
        Assertions.assertDoesNotThrow(library::updateLibrary);
        Assertions.assertTrue(library.isLoaded(), "Windows library isn't loaded");
    }

    private static class TestMacOsLibrary extends MacOSLibrary {
        @Override
        protected void error(final String message, final Throwable e) {
            throw new RuntimeException(e);
        }

        @Override
        protected String getX64Path() {
            return super.getX64Path();
        }

        @Override
        protected String getArm64Path() {
            return super.getArm64Path();
        }

        @Override
        protected List<NativeUtil.Resource> getResourcePaths() {
            return super.getResourcePaths();
        }
    }

    private static class TestWindowsLibrary extends WindowsLibrary {
        @Override
        protected void error(final String message, final Throwable e) {
            throw new RuntimeException(e);
        }

        @Override
        protected String getX64Path() {
            return super.getX64Path();
        }

        @Override
        protected String getX86Path() {
            return super.getX86Path();
        }

        @Override
        protected List<NativeUtil.Resource> getResourcePaths() {
            return super.getResourcePaths();
        }
    }
}
