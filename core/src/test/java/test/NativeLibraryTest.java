/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
package test;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;

import com.github.weisj.darklaf.platform.macos.MacOSLibrary;
import com.github.weisj.darklaf.platform.windows.WindowsLibrary;

public class NativeLibraryTest {

    @Test
    @EnabledOnOs(OS.MAC)
    public void testMacOSLibraryLoading() {
        MacOSLibrary library = new TestMacOsLibrary();
        Assertions.assertNotNull(getClass().getResource(library.getLibraryPath()),
                                 "macOS library doesn't exist");
        Assertions.assertDoesNotThrow(library::updateLibrary);
        Assertions.assertTrue(library.isLoaded(), "MacOS library isn't loaded");
    }

    @Test
    @EnabledOnOs(OS.WINDOWS)
    public void testWindowsLibraryLoading() {
        WindowsLibrary library = new TestWindowsLibrary();
        Assertions.assertNotNull(getClass().getResource(library.getX64Path() + library.getLibraryName()),
                                 "x64 library doesn't exist");
        Assertions.assertNotNull(getClass().getResource(library.getX86Path() + library.getLibraryName()),
                                 "x86 library doesn't exist");
        Assertions.assertDoesNotThrow(library::updateLibrary);
        Assertions.assertTrue(library.isLoaded(), "Windows library isn't loaded");
    }

    private static class TestMacOsLibrary extends MacOSLibrary {
        @Override
        protected void error(final String message, final Throwable e) {
            throw new RuntimeException(e);
        }
    }

    private static class TestWindowsLibrary extends WindowsLibrary {
        @Override
        protected void error(final String message, final Throwable e) {
            throw new RuntimeException(e);
        }
    }
}
