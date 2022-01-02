/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
package com.github.weisj.darklaf.ui.internalframe;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.components.border.DropShadowBorder;

/** @author Jannis Weis */
public class DarkInternalFrameBorder extends DropShadowBorder implements UIResource {

    private final float activeOpacity;
    private final float inactiveOpacity;

    public DarkInternalFrameBorder() {
        activeOpacity = UIManager.getInt("InternalFrame.activeShadowOpacity") / 100f;
        inactiveOpacity = UIManager.getInt("InternalFrame.inactiveShadowOpacity") / 100f;
        int shadowSize = UIManager.getInt("InternalFrame.shadowSize");
        setShadowSize(shadowSize);
        setCornerSize(2 * shadowSize);
        setShadowOpacity(activeOpacity);
        setShadowColor(UIManager.getColor("InternalFrame.borderShadowColor"));
        setShowTopShadow(true);
        setShowBottomShadow(true);
        setShowLeftShadow(true);
        setShowRightShadow(true);
    }

    @Override
    public void paintBorder(final Component c, final Graphics graphics, final int x, final int y, final int width,
            final int height) {
        if (shouldHideShadow(c)) {
            return;
        }
        updateOpacity(c);
        super.paintBorder(c, graphics, x, y, width, height);
    }

    protected void updateOpacity(final Component c) {
        if (c instanceof JInternalFrame && ((JInternalFrame) c).isSelected()) {
            setShadowOpacity(activeOpacity);
        } else {
            setShadowOpacity(inactiveOpacity);
        }
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (shouldHideShadow(c)) {
            return new InsetsUIResource(0, 0, 0, 0);
        }
        updateOpacity(c);
        return super.getBorderInsets(c);
    }

    private boolean shouldHideShadow(final Component c) {
        if (!(c instanceof JInternalFrame) || ((JInternalFrame) c).isMaximum()) {
            return true;
        }
        return !(c.getParent() instanceof JDesktopPane);
    }
}
