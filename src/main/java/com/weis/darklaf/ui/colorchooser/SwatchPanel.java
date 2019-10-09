package com.weis.darklaf.ui.colorchooser;

import com.weis.darklaf.components.alignment.Alignment;
import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * @author Jannis Weis
 */
abstract class SwatchPanel extends JPanel {

    protected Color[] colors;
    protected Dimension swatchSize;
    protected Dimension numSwatches;
    protected Dimension gap;

    private int selRow;
    private int selCol;
    private JToolTip tooltip;

    public SwatchPanel() {
        initValues();
        initColors();
        setToolTipText(""); // register for events
        setOpaque(true);
        setBackground(UIManager.getColor("ColorChooser.swatchGridColor"));
        setFocusable(true);
        setInheritsPopupMenu(true);
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseExited(final MouseEvent e) {
                var p = e.getPoint();
                p = SwingUtilities.convertPoint(e.getComponent(), p, SwatchPanel.this);
                if (tooltip != null && !contains(p)) {
                    tooltip.setVisible(false);
                }
            }
        });
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

    @Nullable
    @Contract(pure = true)
    private Color getColorForCell(final int column, final int row) {
        int index = (row * numSwatches.width) + column;
        if (index >= colors.length) return null;
        return colors[(row * numSwatches.width) + column];
    }

    public void paintComponent(@NotNull final Graphics g) {
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

    @Contract(pure = true)
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

    public String getToolTipText(@NotNull final MouseEvent e) {
        Color color = getColorForLocation(e.getX(), e.getY());
        if (color == null) return null;
        return color.getRed() + ", " + color.getGreen() + ", " + color.getBlue();
    }

    @Override
    public Point getToolTipLocation(final MouseEvent e) {
        if (tooltip == null) {
            createToolTip();
            tooltip.setTipText(getToolTipText(e));
        }
        var p = getCoordinatesForLocation(e.getX(), e.getY());
        int x = getXForColumn(p.x);
        int y = getYForRow(p.y);
        x += swatchSize.width / 2;
        y += swatchSize.height / 2;
        x -= tooltip.getPreferredSize().width / 2;
        return new Point(x, y);
    }

    @Override
    public JToolTip createToolTip() {
        tooltip = super.createToolTip();
//        tooltip.putClientProperty("JToolTip.pointerWidth", 10);
//        tooltip.putClientProperty("JToolTip.pointerHeight", 7);
        tooltip.putClientProperty("JToolTip.pointerLocation", Alignment.NORTH);
        return tooltip;
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

    public Color getColorForLocation(final int x, final int y) {
        var p = getCoordinatesForLocation(x, y);
        return getColorForCell(p.x, p.y);
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
