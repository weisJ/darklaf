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

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.util.logging.Logger;

import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.plaf.synth.ColorType;
import javax.swing.plaf.synth.Region;
import javax.swing.plaf.synth.SynthConstants;
import javax.swing.plaf.synth.SynthContext;
import javax.swing.plaf.synth.SynthGraphicsUtils;
import javax.swing.plaf.synth.SynthStyle;

import sun.swing.SwingUtilities2;

import com.intellij.util.ui.UIUtilities;

public final class SwingUtil {

    private static final Logger LOGGER = LogUtil.getLogger(SwingUtil.class);
    private static final PaintSynthStyle PAINT_STYLE = new PaintSynthStyle();
    private static final SynthGraphicsUtils SYNTH_GRAPHICS_UTILS = new SynthGraphicsUtils();

    private SwingUtil() {}

    private static boolean swingInteropAvailable;

    static {
        try {
            Class.forName("jdk.swing.interop.SwingInterOpUtils");
            swingInteropAvailable = true;
        } catch (Throwable e) {
            swingInteropAvailable = false;
        }
        LOGGER.fine("SwingInterOpUtils available: " + swingInteropAvailable);
    }

    public static void grab(final Toolkit toolkit, final Window window) {
        if (swingInteropAvailable) {
            try {
                Class.forName("jdk.swing.interop.SwingInterOpUtils")
                        .getMethod("grab", Toolkit.class, Window.class)
                        .invoke(null, toolkit, window);
            } catch (Throwable ignored) {
            }
        } else {
            if (toolkit instanceof sun.awt.SunToolkit) {
                ((sun.awt.SunToolkit) toolkit).grab(window);
            }
        }
    }

    public static void ungrab(final Toolkit toolkit, final Window window) {
        if (swingInteropAvailable) {
            try {
                Class.forName("jdk.swing.interop.SwingInterOpUtils")
                        .getMethod("ungrab", Toolkit.class, Window.class)
                        .invoke(null, toolkit, window);
            } catch (Throwable ignored) {
            }
        } else {
            if (toolkit instanceof sun.awt.SunToolkit) {
                ((sun.awt.SunToolkit) toolkit).ungrab(window);
            }
        }
    }

    public static void drawStringUnderlineCharAt(final JComponent c, final Graphics g,
            final String text, final int underlinedIndex, final int x, final int y) {
        FontMetrics fm = SwingUtilities2.getFontMetrics(c, g);
        int adjustedY = y - fm.getAscent();
        SYNTH_GRAPHICS_UTILS.paintText(createSynthContext(c), g, text, x, adjustedY, underlinedIndex);
    }

    public static void drawString(final JComponent c, final Graphics g, final String text, int x, int y) {
        drawStringUnderlineCharAt(c, g, text, -1, x, y);
    }

    public static void setSkipClickCount(final Component comp, int count) {
        UIUtilities.setSkipClickCount(comp, count);
    }

    public static int stringWidth(final JComponent c, final FontMetrics fm, final String string) {
        return UIUtilities.stringWidth(c, fm, string);
    }

    public static String clipStringIfNecessary(final JComponent c, final FontMetrics fm,
            final String string, int availTextWidth) {
        return UIUtilities.clipStringIfNecessary(c, fm, string, availTextWidth);
    }

    @SuppressWarnings("deprecation")
    public static int getFocusAcceleratorKeyMask() {
        if (SystemInfo.isMac) {
            return InputEvent.CTRL_MASK | InputEvent.ALT_MASK;
        } else {
            return InputEvent.ALT_MASK;
        }
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
        UIUtilities.compositeRequestFocus(component);
    }

    public static void setLeadAnchorWithoutSelection(final ListSelectionModel model, int lead, int anchor) {
        UIUtilities.setLeadAnchorWithoutSelection(model, lead, anchor);
    }

    public static boolean pointOutsidePrefSize(JTable table, int row, int column, Point p) {
        return UIUtilities.pointOutsidePrefSize(table, row, column, p);
    }

    public static boolean tabbedPaneChangeFocusTo(final Component comp) {
        return UIUtilities.tabbedPaneChangeFocusTo(comp);
    }

    public static int loc2IndexFileList(final JList<?> list, final Point point) {
        return UIUtilities.loc2IndexFileList(list, point);
    }

    private static SynthContext createSynthContext(final JComponent c) {
        return new SynthContext(c, Region.LABEL, PAINT_STYLE, SynthConstants.DEFAULT);
    }

    private static class PaintSynthStyle extends SynthStyle {

        @Override
        protected Color getColorForState(SynthContext context, ColorType type) {
            return null;
        }

        @Override
        protected Font getFontForState(SynthContext context) {
            return null;
        }
    }
}
