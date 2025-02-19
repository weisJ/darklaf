// Copyright 2000-2020 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0
package com.intellij.util.ui;

import javax.swing.Icon;
import javax.swing.JMenuItem;

public interface MenuItemCheckIconFactory {
    Icon getIcon(JMenuItem var1);

    boolean isCompatible(Object var1, String var2);
}
