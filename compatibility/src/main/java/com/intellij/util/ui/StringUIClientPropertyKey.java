// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0
package com.intellij.util.ui;

public class StringUIClientPropertyKey implements UIClientPropertyKey {
    private final String key;

    public StringUIClientPropertyKey(String key) {
        this.key = key;
    }

    @Override
    public String toString() {
        return this.key;
    }
}
