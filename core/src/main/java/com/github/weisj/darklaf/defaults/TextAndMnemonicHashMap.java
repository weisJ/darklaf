/*
 * MIT License
 *
 * Copyright (c) 2023 Jannis Weis
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
package com.github.weisj.darklaf.defaults;

import java.util.HashMap;

class TextAndMnemonicHashMap extends HashMap<String, Object> {

    private static final String AND_MNEMONIC = "AndMnemonic";
    private static final String TITLE_SUFFIX = ".titleAndMnemonic";
    private static final String TEXT_SUFFIX = ".textAndMnemonic";

    @Override
    public Object get(final Object key) {

        Object value = super.get(key);

        if (value == null) {

            boolean checkTitle = false;

            String stringKey = key.toString();
            String compositeKey = null;

            if (stringKey.endsWith(AND_MNEMONIC)) {
                return null;
            }

            if (stringKey.endsWith(".mnemonic")) {
                compositeKey = composeKey(stringKey, 9, TEXT_SUFFIX);
            } else if (stringKey.endsWith("NameMnemonic")) {
                compositeKey = composeKey(stringKey, 12, TEXT_SUFFIX);
            } else if (stringKey.endsWith("Mnemonic")) {
                compositeKey = composeKey(stringKey, 8, TEXT_SUFFIX);
                checkTitle = true;
            }

            if (compositeKey != null) {
                value = super.get(compositeKey);
                if (value == null && checkTitle) {
                    compositeKey = composeKey(stringKey, 8, TITLE_SUFFIX);
                    value = super.get(compositeKey);
                }

                return value == null ? null : getMnemonicFromProperty(value.toString());
            }

            if (stringKey.endsWith("NameText")) {
                compositeKey = composeKey(stringKey, 8, TEXT_SUFFIX);
            } else if (stringKey.endsWith(".nameText")) {
                compositeKey = composeKey(stringKey, 9, TEXT_SUFFIX);
            } else if (stringKey.endsWith("Text")) {
                compositeKey = composeKey(stringKey, 4, TEXT_SUFFIX);
            } else if (stringKey.endsWith("Title")) {
                compositeKey = composeKey(stringKey, 5, TITLE_SUFFIX);
            }

            if (compositeKey != null) {
                value = super.get(compositeKey);
                return value == null ? null : getTextFromProperty(value.toString());
            }

            if (stringKey.endsWith("DisplayedMnemonicIndex")) {
                compositeKey = composeKey(stringKey, 22, TEXT_SUFFIX);
                value = super.get(compositeKey);
                if (value == null) {
                    compositeKey = composeKey(stringKey, 22, TITLE_SUFFIX);
                    value = super.get(compositeKey);
                }
                return value == null ? null : getIndexFromProperty(value.toString());
            }
        }

        return value;
    }

    private String composeKey(final String key, final int reduce, final String sufix) {
        return key.substring(0, key.length() - reduce) + sufix;
    }

    private String getTextFromProperty(final String text) {
        return text.replace("&", "");
    }

    private String getMnemonicFromProperty(final String text) {
        int index = text.indexOf('&');
        if (0 <= index && index < text.length() - 1) {
            char c = text.charAt(index + 1);
            return Integer.toString((int) Character.toUpperCase(c));
        }
        return null;
    }

    private String getIndexFromProperty(final String text) {
        int index = text.indexOf('&');
        return (index == -1) ? null : Integer.toString(index);
    }
}
