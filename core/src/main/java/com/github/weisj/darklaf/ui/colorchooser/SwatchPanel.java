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
package com.github.weisj.darklaf.ui.colorchooser;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import com.github.weisj.darklaf.components.tooltip.ToolTipContext;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.task.ForegroundColorGenerationTask;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;

/** @author Jannis Weis */
public abstract class SwatchPanel extends JPanel {

    protected Color[] colors;
    protected Dimension swatchSize;
    protected Dimension numSwatches;
    protected Dimension gap;
    private final ToolTipContext toolTipContext = new ToolTipContext(this).setAlignment(Alignment.CENTER)
            .setToolTipRectSupplier(this::getSwatchBounds).setHideOnExit(true);
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
            @Override
            public void focusGained(final FocusEvent e) {
                repaint();
            }

            @Override
            public void focusLost(final FocusEvent e) {
                repaint();
            }
        });
        addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(final KeyEvent e) {
                if (selRow < 0 || selCol < 0) return;
                int typed = e.getKeyCode();
                switch (typed) {
                    case KeyEvent.VK_UP:
                        if (selRow > 0) {
                            setSelection(selRow - 1, selCol);
                        }
                        break;
                    case KeyEvent.VK_DOWN:
                        if (getIndex(selRow + 1, selCol) < colors.length) {
                            setSelection(selRow + 1, selCol);
                        }
                        break;
                    case KeyEvent.VK_LEFT:
                        if (selCol > 0 && SwatchPanel.this.getComponentOrientation().isLeftToRight()) {
                            setSelection(selRow, selCol - 1);
                            repaint();
                        } else if (selCol < numSwatches.width - 1
                                && getSelectionIndex() < colors.length - 1
                                && !SwatchPanel.this.getComponentOrientation().isLeftToRight()) {
                            setSelection(selRow, selCol + 1);
                            repaint();
                        }
                        break;
                    case KeyEvent.VK_RIGHT:
                        if (selCol < numSwatches.width - 1
                                && getSelectionIndex() < colors.length - 1
                                && SwatchPanel.this.getComponentOrientation().isLeftToRight()) {
                            setSelection(selRow, selCol + 1);
                            repaint();
                        } else if (selCol > 0 && !SwatchPanel.this.getComponentOrientation().isLeftToRight()) {
                            setSelection(selRow, selCol - 1);
                            repaint();
                        }
                        break;
                    case KeyEvent.VK_HOME:
                        setSelection(0, 0);
                        break;
                    case KeyEvent.VK_END:
                        setSelection(colors.length - 1);
                        break;
                }
            }
        });
    }

    protected int getSelectionIndex() {
        return getIndex(selRow, selCol);
    }

    protected int getIndex(final int row, final int col) {
        return (row * numSwatches.width) + col;
    }

    protected void setSelection(final int index) {
        int col = index % numSwatches.width;
        int row = (index - col) / numSwatches.width;
        setSelection(row, col);
    }

    protected void setSelection(final int row, final int col) {
        selCol = col;
        selRow = row;
        repaint();
    }

    @Override
    public void updateUI() {
        super.updateUI();
        if (toolTipContext != null) toolTipContext.updateToolTipUI();
    }

    protected void initValues() {}

    protected void initColors() {}

    public Color getSelectedColor() {
        return getColorForCell(selCol, selRow);
    }

    private Color getColorForCell(final int column, final int row) {
        int index = getIndex(row, column);
        if (index >= colors.length || index < 0) return null;
        return colors[index];
    }

    @Override
    public void paintComponent(final Graphics g) {
        Insets ins = getInsets();

        g.setColor(getBackground());
        g.fillRect(ins.left, ins.top, getWidth() - ins.left - ins.right, getHeight() - ins.top - ins.bottom);

        g.translate(ins.left, ins.top);
        for (int row = 0; row < numSwatches.height; row++) {
            int y = getYForRow(row);
            for (int column = 0; column < numSwatches.width; column++) {
                Color c = getColorForCell(column, row);

                if (c == null) continue;

                g.setColor(c);
                int x = getXForColumn(column);
                g.fillRect(x, y, swatchSize.width, swatchSize.height);

                if (selRow == row && selCol == column && this.isFocusOwner()) {
                    Color c2 = ForegroundColorGenerationTask.makeForeground(c);
                    g.setColor(c2);
                    PaintUtil.drawRect(g, x, y, swatchSize.width, swatchSize.height, 1);

                    GraphicsUtil.setupStrokePainting(g);
                    g.drawLine(x + 1, y + 1, x + swatchSize.width - 1, y + swatchSize.height - 1);
                    g.drawLine(x + 1, y + swatchSize.height - 1, x + swatchSize.width - 1, y + 1);
                }
            }
        }
        g.translate(-ins.left, -ins.top);
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

    @Override
    public Dimension getPreferredSize() {
        int x = numSwatches.width * (swatchSize.width + gap.width);
        int y = numSwatches.height * (swatchSize.height + gap.height);
        Insets ins = getInsets();
        return new Dimension(x + ins.left + ins.right, y + ins.top + ins.bottom);
    }

    @Override
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
        Point p = getCoordinatesForLocation(x, y);
        setSelection(p.y, p.x);
    }
}
