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
package com.github.weisj.darklaf.ui.splitpane;

import com.github.weisj.darklaf.decorators.LayoutManagerDelegate;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicSplitPaneDivider;
import javax.swing.plaf.basic.BasicSplitPaneUI;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * @author Jannis Weis
 */
public class DarkSplitPaneUI extends BasicSplitPaneUI implements PropertyChangeListener {

    private static final int DIVIDER_DRAG_SIZE = 10;
    private static final int DIVIDER_DRAG_OFFSET = 5;
    private Style style;
    private Color dividerLine;

    protected DarkSplitPaneUI(final Style style) {
        this.style = style;
    }


    public static ComponentUI createUI(final JComponent c) {
        return new DarkSplitPaneUI(Style.get(UIManager.getString("SplitPane.defaultDividerStyle")));
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        splitPane.setContinuousLayout(true);
        splitPane.setComponentZOrder(getDivider(), 0);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        dividerLine = UIManager.getColor("SplitPane.dividerLineColor");
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        splitPane.addPropertyChangeListener(this);
    }

    @Override
    public void uninstallUI(final JComponent c) {
        if (splitPane.getLayout() instanceof LayoutManagerDelegate) {
            splitPane.setLayout(null);
        }
        super.uninstallUI(c);
        divider = null;
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        splitPane.removePropertyChangeListener(this);
    }

    @Override
    public BasicSplitPaneDivider createDefaultDivider() {
        if (style == Style.GRIP) {
            return new DarkSplitPaneDivider(this);
        } else {
            return new ThinDivider(this);
        }
    }

    @Override
    protected void resetLayoutManager() {
        super.resetLayoutManager();
        splitPane.setLayout(new LayoutManagerDelegate(splitPane.getLayout()) {
            @Override
            public void layoutContainer(final Container parent) {
                super.layoutContainer(parent);
                if (style != Style.GRIP) {
                    Rectangle bounds = getDivider().getBounds();
                    if (getOrientation() == JSplitPane.HORIZONTAL_SPLIT) {
                        bounds.x -= DIVIDER_DRAG_OFFSET;
                        bounds.width = DIVIDER_DRAG_SIZE;
                    } else {
                        bounds.y -= DIVIDER_DRAG_OFFSET;
                        bounds.height = DIVIDER_DRAG_SIZE;
                    }
                    getDivider().setBounds(bounds);
                }
            }
        });
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if ("JSplitPane.style".equals(key)) {
            Object val = evt.getNewValue();
            Style oldStyle = style;
            if (Style.INVISIBLE.name.equals(val)) {
                style = Style.INVISIBLE;
            } else if (Style.LINE.name.equals(val)) {
                style = Style.LINE;
            } else {
                style = Style.GRIP;
            }
            if (oldStyle != style) {
                if (style == Style.GRIP || oldStyle == Style.GRIP) {
                    splitPane.setUI(new DarkSplitPaneUI(style));
                } else {
                    splitPane.doLayout();
                }
            }
        } else if ("orientation".equals(key)) {
            splitPane.doLayout();
        }
    }

    protected Color getDividerLineColor() {
        return dividerLine;
    }

    private enum Style {
        GRIP("grip"),
        LINE("line"),
        INVISIBLE("invisible");

        final private String name;


        Style(final String name) {
            this.name = name;
        }

        static Style get(final String style) {
            try {
                return valueOf(style.toLowerCase());
            } catch (IllegalArgumentException e) {
                return GRIP;
            }
        }
    }

    private final class ThinDivider extends BasicSplitPaneDivider {

        private ThinDivider(final BasicSplitPaneUI ui) {
            super(ui);
        }


        @Override
        public int getDividerSize() {
            return style == Style.LINE ? 1 : 0;
        }

        @Override
        public void paint(final Graphics g) {
            if (style == Style.LINE) {
                g.setColor(getDividerLineColor());
                if (orientation == JSplitPane.HORIZONTAL_SPLIT) {
                    g.drawLine(DIVIDER_DRAG_OFFSET, 0, DIVIDER_DRAG_OFFSET, getHeight());
                } else {
                    g.drawLine(0, DIVIDER_DRAG_OFFSET, getWidth(), DIVIDER_DRAG_OFFSET);
                }
            }
        }

        @Override
        public boolean contains(final Point p) {
            if (!isEnabled()) return false;
            return super.contains(p);
        }

        @Override
        protected void dragDividerTo(final int location) {
            super.dragDividerTo(location + DIVIDER_DRAG_OFFSET);
        }

        @Override
        protected void finishDraggingTo(final int location) {
            super.finishDraggingTo(location + DIVIDER_DRAG_OFFSET);
        }
    }
}
