// Copyright 2000-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.weis.darklaf.ui.colorchooser;

import com.weis.darklaf.components.alignment.Alignment;
import com.weis.darklaf.components.tooltip.ToolTipContext;
import com.weis.darklaf.util.ColorUtil;
import com.weis.darklaf.util.DarkUIUtil;
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
 * @author Jannis Weis
 */
class SlideComponent extends JComponent implements ColorListener {
    private static final int OFFSET = 11;
    private final ToolTipContext toolTipContext = new ToolTipContext(this);
    private final boolean vertical;
    private boolean isOpacity;
    private final String title;
    private final List<Consumer<Integer>> listeners = new ArrayList<>();
    private int pointerValue = 0;
    private int value = 0;
    private Unit unitType = Unit.LEVEL;
    private Color color;
    protected Color borderColor;
    protected Color shadowColor;
    protected Color knobFill;

    SlideComponent(final String title, final boolean vertical, final boolean isOpacity) {
        this.title = title;
        this.vertical = vertical;
        this.isOpacity = isOpacity;
        this.color = Color.WHITE;
        this.borderColor = UIManager.getColor("ColorChooser.sliderBorderColor");
        this.shadowColor = UIManager.getColor("ColorChooser.sliderShadow");
        this.knobFill = UIManager.getColor("ColorChooser.sliderKnobColor");

        toolTipContext.setAlignInside(false)
                .setAlignment(vertical ? Alignment.WEST : Alignment.NORTH)
                .setHideOnExit(false)
                .setToolTipRectSupplier(e -> getKnobRect())
                .setInsets(new Insets(3, 0, 3, 0));

        addMouseMotionListener(new MouseAdapter() {
            @Override
            public void mouseDragged(final MouseEvent e) {
                processMouse(e);
            }
        });

        addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(final MouseEvent e) {
                processMouse(e);
            }
        });


        addMouseWheelListener(event -> {
            int units = event.getUnitsToScroll();
            if (units == 0) return;
            int pointerValue = this.pointerValue + units;
            pointerValue = Math.max(pointerValue, OFFSET);
            int size = this.vertical ? getHeight() : getWidth();
            pointerValue = Math.min(pointerValue, (size - 12));

            this.pointerValue = pointerValue;
            value = pointerValueToValue(this.pointerValue);

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
        int pointerValue = vertical ? e.getY() : e.getX();
        pointerValue = Math.max(pointerValue, OFFSET);
        int size = vertical ? getHeight() : getWidth();
        pointerValue = Math.min(pointerValue, (size - 12));

        this.pointerValue = pointerValue;

        value = pointerValueToValue(this.pointerValue);

        repaint();
        fireValueChanged();
    }

    private int pointerValueToValue(int pointerValue) {
        pointerValue -= OFFSET;
        final int size = vertical ? getHeight() : getWidth();
        double proportion = (size - 23) / 255f;
        return (int) Math.round((pointerValue / proportion));
    }

    private void fireValueChanged() {
        var p = MouseInfo.getPointerInfo().getLocation();
        SwingUtilities.convertPointFromScreen(p, this);
        ToolTipManager.sharedInstance().mouseMoved(new MouseEvent(this, MouseEvent.MOUSE_MOVED, 0,
                                                                  0, p.x, p.y, 0, false, 0));
        for (Consumer<Integer> listener : listeners) {
            listener.accept(value);
        }
    }

    public int getValue() {
        return value;
    }

    public void setValue(final int value) {
        if (value < 0 || value > 255) {
            throw new IllegalArgumentException("Value " + value + " not in range [0,255]");
        }
        pointerValue = valueToPointerValue(value);
        this.value = value;
    }

    private int valueToPointerValue(final int value) {
        final int size = vertical ? getHeight() : getWidth();
        float proportion = (size - 23) / 255f;
        return OFFSET + (int) (value * proportion);
    }

    void setUnits(final Unit unit) {
        unitType = unit;
    }

    public void addListener(final Consumer<Integer> listener) {
        listeners.add(listener);
    }

    @Override
    public void updateUI() {
        super.updateUI();
        borderColor = UIManager.getColor("ColorChooser.sliderBorderColor");
        shadowColor = UIManager.getColor("ColorChooser.sliderShadow");
        knobFill = UIManager.getColor("ColorChooser.sliderKnobColor");
    }

    @Override
    protected void paintComponent(final Graphics g) {
        final Graphics2D g2d = (Graphics2D) g;
        Color endColor = isOpacity ? DarkUIUtil.TRANSPARENT_COLOR : Color.BLACK;
        Color beginColor = color;
        if (vertical) {
            g2d.setPaint(new GradientPaint(0f, 0f, beginColor, 0f, getHeight(), endColor));
            g.fillRect(7, 10, 12, getHeight() - 20);

            g.setColor(borderColor);
            g.fillRect(7, 10, 12, 1);
            g.fillRect(7, 10, 1, getHeight() - 20);
            g.fillRect(7 + 12 - 1, 10, 1, getHeight() - 20);
            g.fillRect(7, 10 + getHeight() - 20 - 1, 12, 1);
        } else {
            g2d.setPaint(new GradientPaint(0f, 0f, endColor, getWidth(), 0f, beginColor));
            g.fillRect(10, 7, getWidth() - 20, 12);

            g.setColor(borderColor);
            g.fillRect(10, 7, 1, 12);
            g.fillRect(10, 7, getWidth() - 20, 1);
            g.fillRect(10, 7 + 12 - 1, getWidth() - 20, 1);
            g.fillRect(10 + getWidth() - 20 - 1, 7, 1, 12);
        }

        drawKnob(g2d, vertical ? 7 : pointerValue, vertical ? pointerValue : 7, vertical);
    }

    @NotNull
    @Contract(" -> new")
    private Rectangle getKnobRect() {
        if (vertical) {
            return new Rectangle(1, pointerValue - 6, 12, 12);
        } else {
            return new Rectangle(pointerValue - 6, 1, 12, 12);
        }
    }

    protected void drawKnob(@NotNull final Graphics2D g2d, int x, int y, final boolean vertical) {
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        if (vertical) {
            y -= 6;
            Polygon arrowShadow = new Polygon();
            arrowShadow.addPoint(x - 5, y + 1);
            arrowShadow.addPoint(x + 7, y + 7);
            arrowShadow.addPoint(x - 5, y + 13);

            g2d.setColor(ColorUtil.toAlpha(shadowColor, 0.5));
            g2d.fill(arrowShadow);

            Polygon arrowHead = new Polygon();
            arrowHead.addPoint(x - 6, y);
            arrowHead.addPoint(x + 6, y + 6);
            arrowHead.addPoint(x - 6, y + 12);

            g2d.setColor(knobFill);
            g2d.fill(arrowHead);
        } else {
            x -= 6;

            Polygon arrowShadow = new Polygon();
            arrowShadow.addPoint(x + 1, y - 5);
            arrowShadow.addPoint(x + 13, y - 5);
            arrowShadow.addPoint(x + 7, y + 7);

            g2d.setColor(ColorUtil.toAlpha(shadowColor, 0.5));
            g2d.fill(arrowShadow);

            Polygon arrowHead = new Polygon();
            arrowHead.addPoint(x, y - 6);
            arrowHead.addPoint(x + 12, y - 6);
            arrowHead.addPoint(x + 6, y + 6);

            g2d.setColor(knobFill);
            g2d.fill(arrowHead);
        }
    }

    @Override
    public Dimension getPreferredSize() {
        return vertical ? new Dimension(22, 100)
                        : new Dimension(100, 22);
    }

    @Override
    public Dimension getMinimumSize() {
        return vertical ? new Dimension(22, 50)
                        : new Dimension(50, 22);
    }

    @Override
    public String getToolTipText(final MouseEvent event) {
        return title + ": " + Unit.formatValue(value, unitType);
    }

    @Override
    public Point getToolTipLocation(final MouseEvent e) {
        return toolTipContext.getToolTipLocation(e);
    }

    @Override
    public JToolTip createToolTip() {
        return toolTipContext.getToolTip();
    }

    @Override
    public void colorChanged(final Color color, final Object source) {
        this.color = ColorUtil.removeAlpha(color);
        repaint();
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