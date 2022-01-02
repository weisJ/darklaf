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
package com.github.weisj.darklaf.components.color;

import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.function.Consumer;
import java.util.function.Function;

import javax.swing.*;

import com.github.weisj.darklaf.components.chooser.ChooserComponent;
import com.github.weisj.darklaf.ui.colorchooser.SwatchPanel;

public class PopupColorSwatchChooser extends SwatchPanel implements ChooserComponent<Color> {

    private final Function<Color, String> tipTextFunction;
    private Color initial;
    private Consumer<Color> callback;

    public PopupColorSwatchChooser(final Color[] colors) {
        this(colors, 5, c -> null);
    }

    public PopupColorSwatchChooser(final Color[] colors, final Function<Color, String> tipTextFunction) {
        this(colors, 5, tipTextFunction);
    }

    public PopupColorSwatchChooser(final Color[] colors, final int columns,
            final Function<Color, String> tipTextFunction) {
        this.colors = colors;
        this.tipTextFunction = tipTextFunction;
        numSwatches.width = columns;
        numSwatches.height = (int) Math.ceil(colors.length / (float) numSwatches.width);
        setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(final MouseEvent e) {
                setSelectedColorFromLocation(e.getX(), e.getY());
            }
        });
    }

    @Override
    public void reset(final Color initial, final Consumer<Color> callback) {
        this.initial = initial;
        this.callback = callback;
        setSelection(-1, -1);
        if (initial != null) {
            for (int i = 0; i < colors.length; i++) {
                if (colors[i].equals(initial)) {
                    setSelection(i);
                    break;
                }
            }
        }
    }

    @Override
    public Color getInitial() {
        return initial;
    }

    @Override
    public Color getSelected() {
        return getSelectedColor();
    }

    @Override
    protected void setSelection(final int row, final int col) {
        super.setSelection(row, col);
        if (callback != null) callback.accept(getSelectedColor());
    }

    @Override
    protected void initValues() {
        swatchSize = UIManager.getDimension("ColorChooser.swatchesSwatchSize", getLocale());
        numSwatches = new Dimension(30, 15);
        gap = new Dimension(1, 1);
    }

    @Override
    public String getToolTipText(final MouseEvent e) {
        String tipText = tipTextFunction.apply(getColorForLocation(e.getX(), e.getY()));
        return tipText != null ? tipText : super.getToolTipText(e);
    }

    @Override
    protected void initColors() {}
}
