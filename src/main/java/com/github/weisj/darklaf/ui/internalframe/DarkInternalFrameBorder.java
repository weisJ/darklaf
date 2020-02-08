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
package com.github.weisj.darklaf.ui.internalframe;

import com.github.weisj.darklaf.components.border.DropShadowBorder;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class DarkInternalFrameBorder extends DropShadowBorder implements UIResource {

    public DarkInternalFrameBorder() {
        super(null, 7, .1f, 9,
              true, true, true, true);
        setShadowColor(UIManager.getColor("InternalFrame.borderShadowColor"));
    }

    @Override
    public void paintBorder(final Component c, @NotNull final Graphics graphics, final int x, final int y,
                            final int width, final int height) {
        if (c instanceof JInternalFrame && ((JInternalFrame) c).isMaximum()) {
            return;
        }
        updateSize(c);
        super.paintBorder(c, graphics, x, y, width, height);
    }

    protected void updateSize(final Component c) {
        if (c instanceof JInternalFrame && ((JInternalFrame) c).isSelected()) {
            setShadowOpacity(0.2f);
        } else {
            setShadowOpacity(0.1f);
        }
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (c instanceof JInternalFrame && ((JInternalFrame) c).isMaximum()) {
            return new InsetsUIResource(0, 0, 0, 0);
        }
        updateSize(c);
        return super.getBorderInsets(c);
    }
}
