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
package com.github.weisj.darklaf.ui.colorchooser;

import com.github.weisj.darklaf.components.tooltip.ToolTipContext;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.GraphicsUtil;
import com.github.weisj.darklaf.utils.Alignment;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * @author Jannis Weis
 */
abstract class SwatchPanel extends JPanel {

    protected Color[] colors;
    protected Dimension swatchSize;
    protected Dimension numSwatches;
    protected Dimension gap;
    private final ToolTipContext toolTipContext = new ToolTipContext(this)
            .setAlignment(Alignment.CENTER)
            .setToolTipRectSupplier(this::getSwatchBounds)
            .setHideOnExit(true);
    private int selRow;
    private int selCol;

    public SwatchPanel() {
        initValues();
        initColors();
        setToolTipText(""); // register for events
        setOpaque(true);
        setBackground(UIManager.getColor("ColorChooser.swatchGridColor"));
        setFocusable(true);
        setInheritsPopupMenu(true);
        addFocusListener(new FocusAdapter() {
            public void focusGained(final FocusEvent e) {
                repaint();
            }

            public void focusLost(final FocusEvent e) {
                repaint();
            }
        });
        addKeyListener(new KeyAdapter() {
            public void keyPressed(final KeyEvent e) {
                int typed = e.getKeyCode();
                switch (typed) {
                    case KeyEvent.VK_UP:
                        if (selRow > 0) {
                            selRow--;
                            repaint();
                        }
                        break;
                    case KeyEvent.VK_DOWN:
                        if (selRow < numSwatches.height - 1) {
                            selRow++;
                            repaint();
                        }
                        break;
                    case KeyEvent.VK_LEFT:
                        if (selCol > 0 && SwatchPanel.this.getComponentOrientation().isLeftToRight()) {
                            selCol--;
                            repaint();
                        } else if (selCol < numSwatches.width - 1
                                && !SwatchPanel.this.getComponentOrientation().isLeftToRight()) {
                            selCol++;
                            repaint();
                        }
                        break;
                    case KeyEvent.VK_RIGHT:
                        if (selCol < numSwatches.width - 1
                                && SwatchPanel.this.getComponentOrientation().isLeftToRight()) {
                            selCol++;
                            repaint();
                        } else if (selCol > 0 && !SwatchPanel.this.getComponentOrientation().isLeftToRight()) {
                            selCol--;
                            repaint();
                        }
                        break;
                    case KeyEvent.VK_HOME:
                        selCol = 0;
                        selRow = 0;
                        repaint();
                        break;
                    case KeyEvent.VK_END:
                        selCol = numSwatches.width - 1;
                        selRow = numSwatches.height - 1;
                        repaint();
                        break;
                }
            }
        });
    }

    protected void initValues() {

    }

    protected void initColors() {
    }

    public Color getSelectedColor() {
        return getColorForCell(selCol, selRow);
    }


    private Color getColorForCell(final int column, final int row) {
        int index = (row * numSwatches.width) + column;
        if (index >= colors.length) return null;
        return colors[(row * numSwatches.width) + column];
    }

    public void paintComponent(final Graphics g) {
        g.setColor(getBackground());
        g.fillRect(0, 0, getWidth(), getHeight());
        for (int row = 0; row < numSwatches.height; row++) {
            int y = getYForRow(row);
            for (int column = 0; column < numSwatches.width; column++) {
                Color c = getColorForCell(column, row);

                g.setColor(c);
                int x = getXForColumn(column);
                g.fillRect(x, y, swatchSize.width, swatchSize.height);

                if (selRow == row && selCol == column && this.isFocusOwner() && c != null) {
                    Color c2 = new Color(255 - c.getRed(), 255 - c.getGreen(), 255 - c.getBlue());
                    g.setColor(c2);
                    DarkUIUtil.drawRect(g, x, y, swatchSize.width, swatchSize.height, 1);

                    GraphicsUtil.setupStrokePainting(g);
                    g.drawLine(x + 1, y + 1, x + swatchSize.width - 1, y + swatchSize.height - 1);
                    g.drawLine(x + 1, y + swatchSize.height - 1, x + swatchSize.width - 1, y + 1);
                }
            }
        }
    }


    private int getYForRow(final int row) {
        return row * (swatchSize.height + gap.height);
    }

    private int getXForColumn(final int column) {
        if (!this.getComponentOrientation().isLeftToRight()) {
            return (numSwatches.width - column - 1) * (swatchSize.width + gap.width);
        } else {
            return column * (swatchSize.width + gap.width);
        }
    }

    public Dimension getPreferredSize() {
        int x = numSwatches.width * (swatchSize.width + gap.width) - 1;
        int y = numSwatches.height * (swatchSize.height + gap.height) - 1;
        return new Dimension(x, y);
    }

    public String getToolTipText(final MouseEvent e) {
        Color color = getColorForLocation(e.getX(), e.getY());
        if (color == null) return null;
        return color.getRed() + ", " + color.getGreen() + ", " + color.getBlue();
    }

    @Override
    public Point getToolTipLocation(final MouseEvent e) {
        return toolTipContext.getToolTipLocation(e);
    }

    @Override
    public JToolTip createToolTip() {
        return toolTipContext.getToolTip();
    }

    public Color getColorForLocation(final int x, final int y) {
        Point p = getCoordinatesForLocation(x, y);
        return getColorForCell(p.x, p.y);
    }

    public Point getCoordinatesForLocation(final int x, final int y) {
        int column;
        if (!this.getComponentOrientation().isLeftToRight()) {
            column = numSwatches.width - x / (swatchSize.width + gap.width) - 1;
        } else {
            column = x / (swatchSize.width + gap.width);
        }
        int row = y / (swatchSize.height + gap.height);
        return new Point(column, row);
    }


    protected Rectangle getSwatchBounds(final MouseEvent e) {
        Point p = getCoordinatesForLocation(e.getX(), e.getY());
        int x = getXForColumn(p.x);
        int y = getYForRow(p.y);
        return new Rectangle(x, y, swatchSize.width, swatchSize.height);
    }

    public void setSelectedColorFromLocation(final int x, final int y) {
        if (!this.getComponentOrientation().isLeftToRight()) {
            selCol = numSwatches.width - x / (swatchSize.width + gap.width) - 1;
        } else {
            selCol = x / (swatchSize.width + gap.width);
        }
        selRow = y / (swatchSize.height + gap.height);
        repaint();
    }
}
