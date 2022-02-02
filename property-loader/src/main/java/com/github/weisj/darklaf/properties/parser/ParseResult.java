/*
 * MIT License
 *
 * Copyright (c) 2019-2022 Jannis Weis
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
package com.github.weisj.darklaf.properties.parser;

import java.util.ArrayList;
import java.util.List;

public class ParseResult {
    public String key;
    public String value;
    public Object result;
    public boolean finished;
    public List<String> warnings = new ArrayList<>();

    private String savedKey;
    private String savedValue;

    public ParseResult(final String key, final String value) {
        this.key = key != null ? key : "";
        this.value = value != null ? value.trim() : "";
    }

    @Override
    public String toString() {
        return "ParseResult{" +
                "key='" + key + '\'' +
                ", value='" + value + '\'' +
                ", result=" + result +
                ", finished=" + finished +
                ", warnings=" + warnings +
                ", savedKey='" + savedKey + '\'' +
                ", savedValue='" + savedValue + '\'' +
                '}';
    }

    public void save() {
        savedKey = key;
        savedValue = value;
    }

    public void restore() {
        key = savedKey;
        value = savedValue;
    }
}
