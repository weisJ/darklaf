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
package com.github.weisj.darklaf.util;

import java.util.Locale;
import java.util.regex.Pattern;

/**
 * Provides basic information about the host jvm and operating system
 *
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public final class SystemInfo {
    public static final String X86 = "32";
    public static final String X64 = "64";
    public static final String OS_NAME = System.getProperty("os.name");
    public static final String OS_VERSION = System.getProperty("os.version").toLowerCase(Locale.ENGLISH);
    public static final String JAVA_VERSION = System.getProperty("java.version");
    public static final String JAVA_HOME = System.getProperty("java.home");
    public static final String JAVA_RUNTIME_VERSION = System.getProperty("java.runtime.version");

    public static final boolean isWindows;
    public static final boolean isOS2;
    public static final boolean isMac;
    public static final boolean isLinux;
    public static final boolean isUnix;

    public static final boolean isFileSystemCaseSensitive;

    public static final boolean isAppleJvm;
    public static final boolean isOracleJvm;
    public static final boolean isSunJvm;

    public static final String jreArchitecture = System.getProperty("sun.arch.data.model");
    public static final boolean isX86;
    public static final boolean isX64;
    public static final boolean undefinedArchitecture;

    public static final String OS_ARCH = System.getProperty("os.arch");
    public static final boolean isX86Compatible;
    public static final boolean isARM;
    public static final boolean isM1;

    public static final boolean isJava9OrGreater;
    public static final boolean isJava16OrGreater;

    private static final String _OS_NAME;

    public static final boolean isMacOSMojave;
    public static final boolean isMacOSCatalina;
    public static final boolean isMacOSYosemite;
    private static boolean isWindows11;
    public static final boolean isWindows10OrGreater;
    public static final boolean isWindows7;
    public static final boolean isWindowsVista;

    static {
        _OS_NAME = OS_NAME.toLowerCase(Locale.ENGLISH);
        isWindows = _OS_NAME.startsWith("windows");
        isOS2 = _OS_NAME.startsWith("os/2") || _OS_NAME.startsWith("os2");
        isMac = _OS_NAME.startsWith("mac");
        isLinux = _OS_NAME.startsWith("linux");
        isUnix = !isWindows && !isOS2;
        isFileSystemCaseSensitive = isUnix && !isMac;
        isAppleJvm = isAppleJvm();
        isOracleJvm = isOracleJvm();
        isSunJvm = isSunJvm();
        isX64 = X64.equals(jreArchitecture);
        isX86 = X86.equals(jreArchitecture);
        undefinedArchitecture = !isX86 && !isX64;
        isARM = OS_ARCH.contains("arm");
        isM1 = isMac && "aarch64".equals(OS_ARCH);
        // This is very much incorrect but will work for most processors.
        isX86Compatible = !isARM && !isM1;
        isMacOSCatalina = isMac && isOsVersionAtLeast("10.15");
        isMacOSMojave = isMacOSCatalina || (isMac && isOsVersionAtLeast("10.14"));
        isMacOSYosemite = isMacOSCatalina || (isMac && isOsVersionAtLeast("10.10"));

        isWindows10OrGreater = isWindows && isOsVersionAtLeast("10.0");
        isWindows11 = isWindows && _OS_NAME.contains("windows 11");

        isWindows7 = isWindows10OrGreater || (isWindows && isOsVersionAtLeast("6.1"));
        isWindowsVista = isWindows7 || (isWindows && isOsVersionAtLeast("6.0"));
        isJava9OrGreater = isJavaVersionAtLeast("9");
        isJava16OrGreater = isJava9OrGreater && isJavaVersionAtLeast("16");
    }

    public static String getOsName() {
        return isMac ? "mac" : isWindows ? "windows" : "linux";
    }

    public static boolean isOsVersionAtLeast(final String version) {
        return compareVersionNumbers(OS_VERSION, version) >= 0;
    }

    public static int compareVersionNumbers(final String v1, final String v2) {
        if (v1 == null && v2 == null) {
            return 0;
        } else if (v1 == null) {
            return -1;
        } else if (v2 == null) {
            return 1;
        } else {
            String[] part1 = v1.split("[.\\_-]");
            String[] part2 = v2.split("[._\\-]");

            int idx;
            for (idx = 0; idx < part1.length && idx < part2.length; ++idx) {
                String p1 = part1[idx];
                String p2 = part2[idx];
                int cmp;
                if (p1.matches("\\d+") && p2.matches("\\d+")) {
                    cmp = Integer.valueOf(p1).compareTo(Integer.valueOf(p2));
                } else {
                    cmp = part1[idx].compareTo(part2[idx]);
                }

                if (cmp != 0) {
                    return cmp;
                }
            }

            if (part1.length == part2.length) {
                return 0;
            } else {
                return part1.length > idx ? 1 : -1;
            }
        }
    }

    public static boolean isWindows11() {
        return isWindows11;
    }

    /**
     * This function is temporary for the time being as the JDK doesn't report whether we are running on
     * Windows 11. Don't call this method if you aren't sure that you are passing the correct value.
     *
     * @param isWindows11 Whether the OS is Windows 11
     */
    public static void setWindows11State(final boolean isWindows11) {
        SystemInfo.isWindows11 = isWindows11;
    }

    public static boolean isJavaVersionAtLeast(final String v) {
        return compareVersionNumbers(JAVA_RUNTIME_VERSION, v) >= 0;
    }

    private static boolean isOracleJvm() {
        String vendor = getJavaVmVendor();
        return vendor != null && containsIgnoreCase(vendor, "Oracle");
    }

    public static String getJavaVmVendor() {
        return System.getProperty("java.vm.vendor");
    }

    private static boolean isSunJvm() {
        String vendor = getJavaVmVendor();
        return vendor != null && containsIgnoreCase(vendor, "Sun") && containsIgnoreCase(vendor, "Microsystems");
    }

    private static boolean isAppleJvm() {
        String vendor = getJavaVmVendor();
        return vendor != null && containsIgnoreCase(vendor, "Apple");
    }

    private static boolean containsIgnoreCase(final String text, final String pattern) {
        return Pattern.compile(pattern, Pattern.LITERAL | Pattern.CASE_INSENSITIVE).matcher(text).find();
    }
}
