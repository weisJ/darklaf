/*
 * MIT License
 *
 * Copyright (c) 2020-2021 Jannis Weis
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
package com.github.weisj.darklaf.ui;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.cell.CellUtil;

public final class DividedWidgetPainter {

    public static void paintBackground(final Graphics2D g, final JComponent c, final int arc,
            final Rectangle splitBounds, final Color bg, final Color splitBg, final WidgetBorderType borderType) {
        int w = c.getWidth();
        int h = c.getHeight();
        boolean ltr = c.getComponentOrientation().isLeftToRight();
        Insets ins = c.getInsets();

        g.setColor(bg);
        fillBackground(g, w, h, arc, ins, borderType);

        if (splitBounds != null) {
            Shape oldClip = g.getClip();
            if (ltr) {
                g.clipRect(splitBounds.x, 0, w - splitBounds.x, h);
            } else {
                g.clipRect(0, 0, splitBounds.x + splitBounds.width, h);
            }

            g.setColor(splitBg);
            fillBackground(g, w, h, arc, ins, borderType);
            g.setClip(oldClip);
        }
    }

    private static void fillBackground(final Graphics2D g, final int w, final int h, final int arc, final Insets ins,
            final WidgetBorderType borderType) {
        if (borderType == WidgetBorderType.Default) {
            PaintUtil.fillRoundRect(g, w, h, ins, arc);
        } else {
            g.fillRect(ins.left, ins.top, w - ins.right - ins.left, h - ins.top - ins.bottom);
        }
    }

    public static void paintBorder(final Graphics2D g, final JComponent c, final int width, final int height,
            final int arc, final int borderSize, final int dividerLocation, final WidgetBorderType borderType,
            final boolean hasFocus, final Color borderColor, final Color focusBorderColor) {
        Insets ins = c.getInsets();
        g.setColor(borderColor);
        if (dividerLocation >= 0) {
            g.fillRect(dividerLocation, ins.top, 1, height - ins.top - ins.bottom);
        }
        if (borderType == WidgetBorderType.None) return;
        if (borderType == WidgetBorderType.Default) {
            if (hasFocus) {
                PaintUtil.paintFocusBorder(g, width, height, arc, borderSize);
                g.setColor(focusBorderColor);
            }
            PaintUtil.paintLineBorder(g, width, height, ins, arc);
        } else {
            paintCellBorder(g, c, width, height, ins, borderType);
        }
    }

    private static void paintCellBorder(final Graphics2D g, final Component c, final int width, final int height,
            final Insets ins, final WidgetBorderType borderType) {
        Component parent = c.getParent();
        if (borderType == WidgetBorderType.Table && parent instanceof JTable) {
            JTable table = (JTable) parent;
            CellUtil.paintTableEditorBorder(g, c, table, width, height);
        } else {
            PaintUtil.drawRect(g, ins.left, ins.top, width - ins.left - ins.right, height - ins.top - ins.bottom,
                    borderType.lineInsets());
        }
    }

    public static WidgetBorderType getBorderType(final Component c,
            final boolean isTableEditor, final boolean isTreeEditor) {
        if (isTableEditor) {
            return WidgetBorderType.Table;
        } else if (isTreeEditor) {
            return WidgetBorderType.Rect;
        } else if (c.getParent() instanceof JPopupMenu) {
            JPopupMenu parent = (JPopupMenu) c.getParent();
            int compCount = parent.getComponentCount();
            if (compCount == 1) return WidgetBorderType.None;

            int widgetIndex = parent.getComponentIndex(c);
            if (widgetIndex == 0) return WidgetBorderType.Bottom;
            boolean showTop = !isWidgetWithBorder(parent.getComponent(widgetIndex - 1));
            boolean showBottom = widgetIndex < compCount - 1;

            if (showTop && showBottom) return WidgetBorderType.TopBottom;
            if (showTop) return WidgetBorderType.Top;
            if (showBottom) return WidgetBorderType.Bottom;
            return WidgetBorderType.None;
        }
        return WidgetBorderType.Default;
    }

    @SuppressWarnings("BooleanMethodIsAlwaysInverted")
    private static boolean isWidgetWithBorder(final Component c) {
        return c instanceof JSpinner || c instanceof JComboBox<?>;
    }

    public enum WidgetBorderType {
        Default {
            @Override
            Insets lineInsets() {
                return null;
            }
        },
        Table {
            @Override
            Insets lineInsets() {
                return new Insets(1, 1, 1, 1);
            }
        },
        Rect {
            @Override
            Insets lineInsets() {
                return new Insets(1, 1, 1, 1);
            }
        },
        None {
            @Override
            Insets lineInsets() {
                return new Insets(0, 0, 0, 0);
            }
        },
        Bottom {
            @Override
            Insets lineInsets() {
                return new Insets(0, 0, 1, 0);
            }
        },
        Top {
            @Override
            Insets lineInsets() {
                return new Insets(1, 0, 0, 0);
            }
        },
        TopBottom {
            @Override
            Insets lineInsets() {
                return new Insets(1, 0, 1, 0);
            }
        };

        abstract Insets lineInsets();
    }
}
