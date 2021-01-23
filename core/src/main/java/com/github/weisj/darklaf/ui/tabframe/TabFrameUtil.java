/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.ui.tabframe;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.components.tabframe.JTabFrame;
import com.github.weisj.darklaf.components.tabframe.TabFrameTab;
import com.github.weisj.darklaf.util.Actions;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.DarkUIUtil;

public final class TabFrameUtil {

    private static final String ACCELERATOR_PREFIX = "accelerator_";

    private TabFrameUtil() {}

    public static void installAccelerator(final JTabFrame tabFrame, final TabFrameTab tab) {
        if (tabFrame == null) return;
        int acc = tab.getAccelerator();
        if (acc < 0) return;
        tabFrame.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(
                KeyStroke.getKeyStroke(UIManager.getString("TabFrame.acceleratorKeyCode") + " " + acc),
                ACCELERATOR_PREFIX + acc);
        tabFrame.getActionMap().put(ACCELERATOR_PREFIX + acc, createAcceleratorAction(tabFrame, tab));
    }

    public static Action createAcceleratorAction(final JTabFrame tabFrame, final TabFrameTab tab) {
        return Actions.create(e -> {
            Alignment a = tab.getOrientation();
            int index = tab.getIndex();
            if (!tab.isSelected()) {
                tabFrame.toggleTab(a, index, true);
            } else {
                Component popup = tabFrame.getPopupComponentAt(a, index);
                if (!DarkUIUtil.hasFocus(popup)) {
                    popup.requestFocusInWindow();
                } else {
                    tabFrame.toggleTab(a, index, false);
                }
            }
        });
    }

    public static void uninstallAccelerator(final JTabFrame tabFrame, final TabFrameTab tab) {
        if (tabFrame == null) return;
        int acc = tab.getAccelerator();
        String accAction = ACCELERATOR_PREFIX + acc;
        tabFrame.getActionMap().remove(accAction);
    }
}
