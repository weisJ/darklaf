/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
package com.github.weisj.darklaf.util;

import java.awt.Component;
import java.awt.Container;
import java.awt.FocusTraversalPolicy;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.event.MouseEvent;

import javax.swing.JComponent;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;

import sun.swing.SwingUtilities2;

public final class SwingUtil {

    private SwingUtil() {}

    public static void drawStringUnderlineCharAt(JComponent c, Graphics g,
            String text, int underlinedIndex, int x, int y) {
        SwingUtilities2.drawStringUnderlineCharAt(c, g, text, underlinedIndex, x, y);
    }

    public static void drawString(final JComponent c, final Graphics g, final String text, int x, int y) {
        SwingUtilities2.drawString(c, g, text, x, y);
    }

    public static void setSkipClickCount(final Component comp, int count) {
        SwingUtilities2.setSkipClickCount(comp, count);
    }

    public static int stringWidth(final JComponent c, final FontMetrics fm, final String string) {
        return SwingUtilities2.stringWidth(c, fm, string);
    }

    public static String clipStringIfNecessary(final JComponent c, final FontMetrics fm,
            final String string, int availTextWidth) {
        return SwingUtilities2.clipStringIfNecessary(c, fm, string, availTextWidth);
    }

    public static FontMetrics getFontMetrics(final JComponent c, final Graphics g) {
        if (g == null) throw new IllegalArgumentException("Graphics must not be null");
        return getFontMetrics(c, g.getFont());
    }

    public static FontMetrics getFontMetrics(final JComponent c, final Font font) {
        if (c == null) throw new IllegalArgumentException("Component must not be null");
        if (font == null) throw new IllegalArgumentException("Font must not be null");
        // Note: We assume that we're using the FontMetrics
        // from the widget to layout out text, otherwise we can get
        // mismatches when printing.
        return c.getFontMetrics(font);
    }

    public static boolean shouldIgnore(final MouseEvent me, final JComponent c) {
        return c == null || !c.isEnabled()
                || !SwingUtilities.isLeftMouseButton(me)
                || me.isConsumed();
    }

    public static void adjustFocus(JComponent c) {
        if (!c.hasFocus() && c.isRequestFocusEnabled()) {
            c.requestFocus();
        }
    }

    public static void compositeRequestFocus(final Component component) {
        if (component instanceof Container) {
            Container container = (Container) component;
            if (container.isFocusCycleRoot()) {
                FocusTraversalPolicy policy = container.getFocusTraversalPolicy();
                Component comp = policy.getDefaultComponent(container);
                if (comp != null) {
                    comp.requestFocus();
                    return;
                }
            }
            Container rootAncestor = container.getFocusCycleRootAncestor();
            if (rootAncestor != null) {
                FocusTraversalPolicy policy = rootAncestor.getFocusTraversalPolicy();
                Component comp = policy.getComponentAfter(rootAncestor, container);

                if (comp != null && SwingUtilities.isDescendingFrom(comp, container)) {
                    comp.requestFocus();
                    return;
                }
            }
        }
        if (component.isFocusable()) {
            component.requestFocus();
        }
    }

    public static void setLeadAnchorWithoutSelection(final ListSelectionModel model, int lead, int anchor) {
        if (anchor == -1) {
            anchor = lead;
        }
        if (lead == -1) {
            model.setAnchorSelectionIndex(-1);
            model.setLeadSelectionIndex(-1);
        } else {
            if (model.isSelectedIndex(lead)) {
                model.addSelectionInterval(lead, lead);
            } else {
                model.removeSelectionInterval(lead, lead);
            }
            model.setAnchorSelectionIndex(anchor);
        }
    }
}
