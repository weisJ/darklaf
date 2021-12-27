/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
#include "Registry.h"

DWORD RegGetDword(HKEY hKey, const LPCSTR subKey, const LPCSTR value) {
    DWORD data {};
    DWORD dataSize = sizeof(data);
    DWORD flags = RRF_RT_REG_DWORD;
    ModifyFlags(flags);
    LONG retCode = ::RegGetValueA(hKey, subKey, value, flags, nullptr, &data, &dataSize);
    if (retCode != ERROR_SUCCESS) throw retCode;

    return data;
}

std::string RegGetString(HKEY hKey, const LPCSTR subKey, const LPCSTR value) {
    DWORD dataSize {};
    DWORD flags = RRF_RT_REG_SZ;
    ModifyFlags(flags);
    LONG retCode = ::RegGetValueA(hKey, subKey, value, flags, nullptr, nullptr, &dataSize);
    if (retCode != ERROR_SUCCESS) throw retCode;

    std::string data;
    DWORD stringLengthInChars = dataSize / sizeof(char);
    data.resize(stringLengthInChars);
    retCode = ::RegGetValueA(hKey, subKey, value, flags, nullptr, &data[0], &dataSize);
    if (retCode != ERROR_SUCCESS) throw retCode;

    return data;
}
