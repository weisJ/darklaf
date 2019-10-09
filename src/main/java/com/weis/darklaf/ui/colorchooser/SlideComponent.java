package com.weis.darklaf.ui.colorchooser;

import com.weis.darklaf.components.alignment.Alignment;
import com.weis.darklaf.util.ColorUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

/**
 * @author Alexey Pegov
 * @author Konstantin Bulenkov
 */
class SlideComponent extends JComponent {
    private static final int OFFSET = 11;
    private final boolean myVertical;
    private final String myTitle;
    private final List<Consumer<Integer>> myListeners = new ArrayList<>();
    private int myPointerValue = 0;
    private int myValue = 0;
    private Unit myUnit = Unit.LEVEL;
    private JToolTip tooltip;

    SlideComponent(final String title, final boolean vertical) {
        myTitle = title;
        myVertical = vertical;

        addMouseMotionListener(new MouseAdapter() {
            @Override
            public void mouseDragged(final MouseEvent e) {
                processMouse(e);
            }
        });

        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseExited(final MouseEvent e) {
                var p = e.getPoint();
                p = SwingUtilities.convertPoint(e.getComponent(), p, SlideComponent.this);
                if (tooltip != null && !contains(p)) {
                    tooltip.setVisible(false);
                }
            }
        });

        addMouseWheelListener(event -> {
            int units = event.getUnitsToScroll();
            if (units == 0) return;
            int pointerValue = myPointerValue + units;
            pointerValue = Math.max(pointerValue, OFFSET);
            int size = myVertical ? getHeight() : getWidth();
            pointerValue = Math.min(pointerValue, (size - 12));

            myPointerValue = pointerValue;
            myValue = pointerValueToValue(myPointerValue);

            repaint();
            fireValueChanged();
        });

        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(final ComponentEvent e) {
                setValue(getValue());
                fireValueChanged();
                repaint();
            }
        });

        setToolTipText(getToolTipText(null));
    }

    private void processMouse(final MouseEvent e) {
        int pointerValue = myVertical ? e.getY() : e.getX();
        pointerValue = Math.max(pointerValue, OFFSET);
        int size = myVertical ? getHeight() : getWidth();
        pointerValue = Math.min(pointerValue, (size - 12));

        myPointerValue = pointerValue;

        myValue = pointerValueToValue(myPointerValue);

        repaint();
        fireValueChanged();
    }

    private int pointerValueToValue(int pointerValue) {
        pointerValue -= OFFSET;
        final int size = myVertical ? getHeight() : getWidth();
        double proportion = (size - 23) / 255f;
        return (int) Math.round((pointerValue / proportion));
    }

    private void fireValueChanged() {
        for (Consumer<Integer> listener : myListeners) {
            listener.accept(myValue);
        }
    }

    public int getValue() {
        return myValue;
    }

    public void setValue(final int value) {
        if (value < 0 || value > 255) {
            throw new IllegalArgumentException("Value " + value + " not in range [0,255]");
        }
        myPointerValue = valueToPointerValue(value);
        myValue = value;
    }

    private int valueToPointerValue(final int value) {
        final int size = myVertical ? getHeight() : getWidth();
        float proportion = (size - 23) / 255f;
        return OFFSET + (int) (value * proportion);
    }

    void setUnits(final Unit unit) {
        myUnit = unit;
    }

    public void addListener(final Consumer<Integer> listener) {
        myListeners.add(listener);
    }

    @Override
    protected void paintComponent(final Graphics g) {
        final Graphics2D g2d = (Graphics2D) g;

        if (myVertical) {
            g2d.setPaint(new GradientPaint(0f, 0f, Color.WHITE, 0f, getHeight(), Color.BLACK));
            g.fillRect(7, 10, 12, getHeight() - 20);

            g.setColor(UIManager.getColor("ColorChooser.sliderBorderColor"));
            g.fillRect(7, 10, 12, 1);
            g.fillRect(7, 10, 1, getHeight() - 20);
            g.fillRect(7 + 12 - 1, 10, 1, getHeight() - 20);
            g.fillRect(7, 10 + getHeight() - 20 - 1, 12, 1);
        } else {
            g2d.setPaint(new GradientPaint(0f, 0f, Color.WHITE, getWidth(), 0f, Color.BLACK));
            g.fillRect(10, 7, getWidth() - 20, 12);

            g.setColor(UIManager.getColor("ColorChooser.sliderBorderColor"));
            g.fillRect(10, 7, 1, 12);
            g.fillRect(10, 7, getWidth() - 20, 1);
            g.fillRect(10, 7 + 12 - 1, getWidth() - 20, 1);
            g.fillRect(10 + getWidth() - 20 - 1, 7, 1, 12);
        }

        drawKnob(g2d, myVertical ? 7 : myPointerValue, myVertical ? myPointerValue : 7, myVertical);
    }

    private static void drawKnob(@NotNull final Graphics2D g2d, int x, int y, final boolean vertical) {
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        if (vertical) {
            y -= 6;
            Polygon arrowShadow = new Polygon();
            arrowShadow.addPoint(x - 5, y + 1);
            arrowShadow.addPoint(x + 7, y + 7);
            arrowShadow.addPoint(x - 5, y + 13);

            g2d.setColor(ColorUtil.toAlpha(UIManager.getColor("ColorChooser.sliderShadow"), 0.5));
            g2d.fill(arrowShadow);

            Polygon arrowHead = new Polygon();
            arrowHead.addPoint(x - 6, y);
            arrowHead.addPoint(x + 6, y + 6);
            arrowHead.addPoint(x - 6, y + 12);

            g2d.setColor(UIManager.getColor("ColorChooser.sliderKnobColor"));
            g2d.fill(arrowHead);
        } else {
            x -= 6;

            Polygon arrowShadow = new Polygon();
            arrowShadow.addPoint(x + 1, y - 5);
            arrowShadow.addPoint(x + 13, y - 5);
            arrowShadow.addPoint(x + 7, y + 7);

            g2d.setColor(ColorUtil.toAlpha(UIManager.getColor("ColorChooser.sliderShadow"), 0.5));
            g2d.fill(arrowShadow);

            Polygon arrowHead = new Polygon();
            arrowHead.addPoint(x, y - 6);
            arrowHead.addPoint(x + 12, y - 6);
            arrowHead.addPoint(x + 6, y + 6);

            g2d.setColor(UIManager.getColor("ColorChooser.sliderKnobColor"));
            g2d.fill(arrowHead);
        }
    }

    @Override
    public Dimension getPreferredSize() {
        return myVertical ? new Dimension(22, 100)
                          : new Dimension(100, 22);
    }

    @Override
    public Dimension getMinimumSize() {
        return myVertical ? new Dimension(22, 50)
                          : new Dimension(50, 22);
    }

    @Override
    public String getToolTipText(final MouseEvent event) {
        return myTitle + ": " + Unit.formatValue(myValue, myUnit);
    }

    @Override
    public Point getToolTipLocation(final MouseEvent e) {
        if (tooltip == null) {
            createToolTip();
            tooltip.setTipText(getToolTipText(e));
        }
        final Point point = myVertical ? new Point(0, myPointerValue) : new Point(myPointerValue, 0);
        if (myVertical) {
            point.x -= tooltip.getPreferredSize().width - 7;
            point.y -= (tooltip.getPreferredSize().height) / 2 - 4;
        } else {
            point.x -= tooltip.getPreferredSize().width / 2;
            point.y -= tooltip.getPreferredSize().height - 7;
        }
        return point;
    }

    @Override
    public JToolTip createToolTip() {
        tooltip = super.createToolTip();
        if (myVertical) {
            tooltip.setPreferredSize(new Dimension(130, 39));
        } else {
            tooltip.setPreferredSize(new Dimension(120, 46));
        }
        tooltip.putClientProperty("JToolTip.insets", new Insets(3, 0, 3, 0));
        tooltip.putClientProperty("JToolTip.pointerLocation", myVertical ? Alignment.EAST : Alignment.SOUTH);
        return tooltip;
    }

    enum Unit {
        PERCENT,
        LEVEL;

        private static final float PERCENT_MAX_VALUE = 100f;
        private static final float LEVEL_MAX_VALUE = 255f;

        private static String formatValue(final int value, final Unit unit) {
            if (unit == PERCENT) {
                return String.format("%d%s", (int) ((getMaxValue(unit) / LEVEL_MAX_VALUE * value)), "%");
            } else {
                return String.format("%d", (int) (LEVEL_MAX_VALUE - ((getMaxValue(unit) / LEVEL_MAX_VALUE * value))));
            }
        }

        @Contract(pure = true)
        private static float getMaxValue(final Unit unit) {
            return LEVEL.equals(unit) ? LEVEL_MAX_VALUE : PERCENT_MAX_VALUE;
        }
    }
}