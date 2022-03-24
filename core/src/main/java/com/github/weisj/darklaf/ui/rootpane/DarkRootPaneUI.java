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
package com.github.weisj.darklaf.ui.rootpane;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.nativelaf.DecorationsHandler;
import com.github.weisj.darklaf.platform.decorations.AbstractNativeDecorationsRootPaneUI;
import com.github.weisj.darklaf.util.PropertyUtil;

/** @author Jannis Weis */
public class DarkRootPaneUI extends AbstractNativeDecorationsRootPaneUI {

    private static final String[] borderKeys = new String[] {"RootPane.border", "RootPane.frameBorder"};

    public DarkRootPaneUI() {
        super(DecorationsHandler.getSharedInstance());
    }

    public static ComponentUI createUI(final JComponent comp) {
        return new DarkRootPaneUI();
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        installBorder((JRootPane) c);
    }

    @Override
    protected void installDefaults(final JRootPane c) {
        super.installDefaults(c);
        PropertyUtil.installBooleanProperty(c, KEY_UNIFIED_MENUBAR, "TitlePane.unifiedMenuBar");
        PropertyUtil.installBooleanProperty(c, KEY_COLORED_TITLE_BAR, "macos.coloredTitleBar");
        LookAndFeel.installColors(c, "RootPane.background", "RootPane.foreground");
    }

    protected void installBorder(final JRootPane root) {
        if (root == null) return;
        LookAndFeel.installBorder(root, borderKeys[Math.max(Math.min(1, windowDecorationsStyle), 0)]);
    }

    private static void uninstallBorder(final JRootPane root) {
        LookAndFeel.uninstallBorder(root);
    }

    @Override
    protected void onDecorationsUninstall(JRootPane rootPane) {
        uninstallBorder(rootPane);
    }

    @Override
    protected void onDecorationsInstall(JRootPane rootPane) {
        installBorder(rootPane);
    }

    @Override
    protected boolean shouldRemoveDecorations(JRootPane rootPane) {
        return !LafManager.isInstalled() || !LafManager.isDecorationsEnabled();
    }
}
