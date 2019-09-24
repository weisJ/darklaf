package com.weis.darklaf.ui.splitpane;

import com.weis.darklaf.decorators.LayoutManagerDelegate;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicSplitPaneDivider;
import javax.swing.plaf.basic.BasicSplitPaneUI;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class DarkSplitPaneUI extends BasicSplitPaneUI implements PropertyChangeListener {

    private static final int DIVIDER_DRAG_SIZE = 10;
    private static final int DIVIDER_DRAG_OFFSET = 5;
    private int defaultDividerSize;
    private Style style;

    protected DarkSplitPaneUI(final Style style) {
        this.style = style;
    }

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkSplitPaneUI(Style.DEFAULT);
    }

    @Override
    public BasicSplitPaneDivider createDefaultDivider() {
        if (style == Style.DEFAULT) {
            return new DarkSplitPaneDivider(this);
        } else {
            return new ThinDivider(this);
        }
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        defaultDividerSize = splitPane.getDividerSize();
        if (style == Style.LINE) {
            splitPane.setDividerSize(1);
        } else if (style == Style.INVISIBLE) {
            splitPane.setDividerSize(0);
        }
        splitPane.setContinuousLayout(true);
    }

    @Override
    public void uninstallUI(final JComponent c) {
        if (splitPane.getLayout() instanceof LayoutManagerDelegate) {
            splitPane.setLayout(null);
        }
        super.uninstallUI(c);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        splitPane.addPropertyChangeListener(this);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        splitPane.removePropertyChangeListener(this);
    }

    @Override
    public void propertyChange(@NotNull final PropertyChangeEvent evt) {
        var key = evt.getPropertyName();
        if ("JSplitPane.style".equals(key)) {
            var val = evt.getNewValue();
            Style oldStyle = style;
            if (Style.INVISIBLE.name.equals(val)) {
                style = Style.INVISIBLE;
            } else if (Style.LINE.name.equals(val)) {
                style = Style.LINE;
            } else {
                style = Style.DEFAULT;
            }
            if (oldStyle != style) {
                if (style == Style.DEFAULT || oldStyle == Style.DEFAULT) {
                    splitPane.setDividerSize(defaultDividerSize);
                    splitPane.remove(divider);
                    divider = createDefaultDivider();
                    Border b = divider.getBorder();
                    if (!(b instanceof UIResource)) {
                        divider.setBorder(UIManager.getBorder("SplitPaneDivider.border"));
                    }
                    Integer temp = (Integer) UIManager.get("SplitPane.dividerSize");
                    LookAndFeel.installProperty(splitPane, "dividerSize", temp == null ? 10 : temp);

                    divider.setDividerSize(splitPane.getDividerSize());
                    dividerSize = divider.getDividerSize();
                    splitPane.add(divider, JSplitPane.DIVIDER);
                    splitPane.invalidate();
                }
            }
        }
    }

    @Override
    protected void resetLayoutManager() {
        super.resetLayoutManager();
        splitPane.setLayout(new LayoutManagerDelegate(splitPane.getLayout()) {
            @Override
            public void layoutContainer(final Container parent) {
                super.layoutContainer(parent);
                if (style != Style.DEFAULT) {
                    var bounds = getDivider().getBounds();
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

    protected Color getDividerLineColor() {
        return UIManager.getColor("SplitPane.dividerLineColor");
    }

    private enum Style {
        DEFAULT("default"),
        LINE("line"),
        INVISIBLE("invisible");

        final private String name;

        @Contract(pure = true)
        Style(final String name) {
            this.name = name;
        }
    }

    private final class ThinDivider extends BasicSplitPaneDivider {

        private ThinDivider(@NotNull final BasicSplitPaneUI ui) {
            super(ui);
        }

        @Override
        public void paint(@NotNull final Graphics g) {
            if (style == Style.LINE) {
                g.setColor(getDividerLineColor());
                if (orientation == JSplitPane.HORIZONTAL_SPLIT) {
                    g.drawLine(DIVIDER_DRAG_OFFSET, 0, DIVIDER_DRAG_OFFSET, getHeight() - 1);
                } else {
                    g.drawLine(0, DIVIDER_DRAG_OFFSET, getWidth() - 1, DIVIDER_DRAG_OFFSET);
                }
            }
        }

        @Contract(pure = true)
        @Override
        public int getDividerSize() {
            return style == Style.LINE ? 1 : 0;
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
