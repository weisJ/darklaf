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
#include <winuser.h>

#define DARK_MODE_PATH ("Software\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize")
#define DARK_MODE_KEY ("AppsUseLightTheme")

#define FONT_SCALE_PATH ("Software\\Microsoft\\Accessibility")
#define FONT_SCALE_KEY ("TextScaleFactor")

#define HIGH_CONTRAST_PATH ("Control Panel\\Accessibility\\HighContrast")
#define HIGH_CONTRAST_THEME_KEY ("High Contrast Scheme")
#define HIGH_CONTRAST_LIGHT_THEME ("High Contrast White")

#define DARK_MODE_DEFAULT_VALUE false
#define HIGH_CONTRAST_DEFAULT_VALUE false
#define FONT_SCALE_DEFAULT_VALUE 100


void ModifyFlags(DWORD &flags)
{
#ifdef _WIN64
    flags |= RRF_SUBKEY_WOW6464KEY;
#else
    flags |= RRF_SUBKEY_WOW6432KEY;
#endif
}

DWORD RegGetDword(HKEY hKey, const LPCSTR subKey, const LPCSTR value)
{
    DWORD data{};
    DWORD dataSize = sizeof(data);
    DWORD flags = RRF_RT_REG_DWORD;
    ModifyFlags(flags);
    LONG retCode = ::RegGetValue(hKey, subKey, value, flags, nullptr, &data, &dataSize);
    if (retCode != ERROR_SUCCESS) throw retCode;

    return data;
}

std::string RegGetString(HKEY hKey, const LPCSTR subKey, const LPCSTR value)
{
    DWORD dataSize{};
    DWORD flags = RRF_RT_REG_SZ;
    ModifyFlags(flags);
    LONG retCode = ::RegGetValue(hKey, subKey, value, flags, nullptr, nullptr, &dataSize);
    if (retCode != ERROR_SUCCESS) throw retCode;

    std::string data;
    DWORD stringLengthInChars = dataSize / sizeof(char);
    data.resize(stringLengthInChars);
    retCode = ::RegGetValue(hKey, subKey, value, flags, nullptr, &data[0], &dataSize);
    if (retCode != ERROR_SUCCESS) throw retCode;

    return data;
}

bool IsHighContrastMode()
{
    HIGHCONTRAST info = { 0 };
    info.cbSize = sizeof(HIGHCONTRAST);
    BOOL ok = SystemParametersInfo(SPI_GETHIGHCONTRAST, 0, &info, 0);
    if (ok)
    {
        return info.dwFlags & HCF_HIGHCONTRASTON;
    }
    return HIGH_CONTRAST_DEFAULT_VALUE;
}

bool IsDarkMode()
{
    try
    {
        bool appsUseDark = (0 == RegGetDword(HKEY_CURRENT_USER, DARK_MODE_PATH, DARK_MODE_KEY));
        bool isHighContrast = IsHighContrastMode();
        if (!isHighContrast) return appsUseDark;

        std::string themeValue = RegGetString(HKEY_CURRENT_USER, HIGH_CONTRAST_PATH, HIGH_CONTRAST_THEME_KEY);
        return (strcmp(themeValue.c_str(), HIGH_CONTRAST_LIGHT_THEME) != 0);
    }
    catch (LONG e)
    {
        return DARK_MODE_DEFAULT_VALUE;
    }
}

unsigned int GetTextScaleFactor()
{
    try
    {
        return RegGetDword(HKEY_CURRENT_USER, FONT_SCALE_PATH, FONT_SCALE_KEY);
    }
    catch (LONG e)
    {
        return FONT_SCALE_DEFAULT_VALUE;
    }
}

bool RegisterRegistryEvent(const LPCSTR subKey, HANDLE event)
{
    HKEY hKey;
    REGSAM flags = KEY_NOTIFY;
    ModifyFlags(flags);
    DWORD res = RegOpenKeyEx(HKEY_CURRENT_USER, subKey, 0, flags, &hKey);
    if (res == ERROR_SUCCESS)
    {
        LSTATUS status = RegNotifyChangeKeyValue(hKey, FALSE, REG_NOTIFY_CHANGE_LAST_SET, event, TRUE);
        return status == ERROR_SUCCESS;
    }
    else
    {
        return false;
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

JNIEXPORT jlong JNICALL
Java_com_github_weisj_darklaf_platform_windows_JNIThemeInfoWindows_createEventHandle(JNIEnv *env, jclass obj)
{
    return (jlong) CreateEvent(NULL, FALSE, FALSE, NULL);
}

JNIEXPORT void JNICALL
Java_com_github_weisj_darklaf_platform_windows_JNIThemeInfoWindows_notifyEventHandle(JNIEnv *env, jclass obj, jlong eventHandle)
{
    SetEvent(reinterpret_cast<HANDLE>(eventHandle));
}

JNIEXPORT jboolean JNICALL
Java_com_github_weisj_darklaf_platform_windows_JNIThemeInfoWindows_waitPreferenceChange(JNIEnv *env, jclass obj, jlong eventHandle)
{
    HANDLE event = reinterpret_cast<HANDLE>(eventHandle);
    if (!RegisterRegistryEvent(FONT_SCALE_PATH, event)) return (jboolean) false;
    if (!RegisterRegistryEvent(DARK_MODE_PATH, event)) return (jboolean) false;
    if (!RegisterRegistryEvent(HIGH_CONTRAST_PATH, event)) return (jboolean) false;
    return (jboolean) (WaitForSingleObject(event, INFINITE) != WAIT_FAILED);
}
