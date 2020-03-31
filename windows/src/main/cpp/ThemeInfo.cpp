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
 */
#include "com_github_weisj_darklaf_platform_windows_JNIThemeInfoWindows.h"

#include <iostream>
#include <string>
#include <windows.h>
#include <winreg.h>

#define DARK_MODE_PATH "Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize"
#define DARK_MODE_KEY "AppsUseLightTheme"
#define FONT_SCALE_PATH "Software\\Microsoft\\Accessibility"
#define FONT_SCALE_KEY "TextScaleFactor"

DWORD RegGetDword(HKEY hKey, const LPCSTR subKey, const LPCSTR value)
{
    DWORD data{};
    DWORD dataSize = sizeof(data);
    DWORD flags = RRF_RT_REG_DWORD;
#ifdef _WIN64
    flags |= RRF_SUBKEY_WOW6464KEY;
#else
    flags |= RRF_SUBKEY_WOW6432KEY;
#endif
    LONG retCode = ::RegGetValue(hKey, subKey, value, flags, nullptr, &data, &dataSize);
    if (retCode != ERROR_SUCCESS)
    {
        throw retCode;
    }
    return data;
}

bool IsDarkMode()
{
    try
    {
        DWORD data = RegGetDword(HKEY_CURRENT_USER, DARK_MODE_PATH, DARK_MODE_KEY);
        return data == 0;
    }
    catch (LONG e)
    {
        // Error. Simply return false.
        return false;
    }
}

bool IsHighContrastMode()
{
    HIGHCONTRAST info = { 0 };
    info.cbSize = sizeof(HIGHCONTRAST);
    BOOL ok = SystemParametersInfoW(SPI_GETHIGHCONTRAST, 0, &info, 0);
    if (ok)
    {
        return info.dwFlags & HCF_HIGHCONTRASTON;
    }
    return false;
}

unsigned int GetTextScaleFactor()
{
    try
    {
        return RegGetDword(HKEY_CURRENT_USER, FONT_SCALE_PATH, FONT_SCALE_KEY);
    }
    catch (LONG e)
    {
        return 100;
    }
}

JNIEXPORT jboolean JNICALL
Java_com_github_weisj_darklaf_platform_windows_JNIThemeInfoWindows_isDarkThemeEnabled(JNIEnv *env, jclass obj)
{
    return (jboolean)IsDarkMode();
}

JNIEXPORT jboolean JNICALL
Java_com_github_weisj_darklaf_platform_windows_JNIThemeInfoWindows_isHighContrastEnabled(JNIEnv *env, jclass obj)
{
    return (jboolean)IsHighContrastMode();
}

JNIEXPORT jlong JNICALL
Java_com_github_weisj_darklaf_platform_windows_JNIThemeInfoWindows_getFontScaleFactor(JNIEnv *env, jclass obj)
{
    return (jlong)GetTextScaleFactor();
}

