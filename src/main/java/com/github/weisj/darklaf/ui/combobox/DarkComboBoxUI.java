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
package com.github.weisj.darklaf.ui.combobox;

import com.github.weisj.darklaf.components.ArrowButton;
import com.github.weisj.darklaf.ui.list.DarkListCellRenderer;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.GraphicsContext;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.basic.BasicComboBoxUI;
import javax.swing.plaf.basic.ComboPopup;
import java.awt.*;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.Area;
import java.awt.geom.RoundRectangle2D;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkComboBoxUI extends BasicComboBoxUI implements Border {

    private static final int BUTTON_PAD = 7;

    private final MouseListener mouseListener = new MouseAdapter() {
        @Override
        public void mousePressed(final MouseEvent e) {
            super.mousePressed(e);
            comboBox.getEditor().getEditorComponent().requestFocus();
        }
    };
    protected int arcSize;
    protected int borderSize;
    protected Color background;
    protected Color editBackground;
    protected Color inactiveBackground;
    protected Color inactiveForeground;
    protected Color focusBorderColor;
    protected Color borderColor;
    protected Color inactiveBorderColor;
    protected Color arrowBackgroundStart;
    protected Color arrowBackgroundEnd;
    private Insets boxPadding;

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkComboBoxUI();
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        comboBox.setBorder(this);
        boxPadding = UIManager.getInsets("ComboBox.padding");
        borderSize = UIManager.getInt("ComboBox.borderThickness");
        background = UIManager.getColor("ComboBox.activeBackground");
        editBackground = UIManager.getColor("ComboBox.editBackground");
        inactiveBackground = UIManager.getColor("ComboBox.inactiveBackground");
        inactiveForeground = UIManager.getColor("ComboBox.disabledForeground");
        focusBorderColor = UIManager.getColor("ComboBox.focusBorderColor");
        borderColor = UIManager.getColor("ComboBox.activeBorderColor");
        inactiveBorderColor = UIManager.getColor("ComboBox.inactiveBorderColor");
        arrowBackgroundStart = UIManager.getColor("ComboBox.arrowBackgroundStart");
        arrowBackgroundEnd = UIManager.getColor("ComboBox.arrowBackgroundEnd");
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        LookAndFeel.installProperty(comboBox, "opaque", false);
        arcSize = UIManager.getInt("ComboBox.arc");
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        comboBox.addMouseListener(mouseListener);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        comboBox.removeMouseListener(mouseListener);
    }

    @Override
    protected ComboPopup createPopup() {
        return new DarkComboPopup(comboBox);
    }

    @Override
    protected ListCellRenderer<Object> createRenderer() {
        return new DarkListCellRenderer();
    }

    @Override
    protected ComboBoxEditor createEditor() {
        final ComboBoxEditor comboBoxEditor = super.createEditor();
        Component comp = comboBoxEditor.getEditorComponent();
        comp.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(final KeyEvent e) {
                process(e);
            }

            private void process(@NotNull final KeyEvent e) {
                final int code = e.getKeyCode();
                if ((code == KeyEvent.VK_UP || code == KeyEvent.VK_DOWN) && e.getModifiersEx() == 0) {
                    comboBox.dispatchEvent(e);
                }
            }

            @Override
            public void keyReleased(final KeyEvent e) {
                process(e);
            }
        });
        comp.addFocusListener(new FocusAdapter() {
            @Override
            public void focusGained(final FocusEvent e) {
                comboBox.revalidate();
                comboBox.repaint();
            }

            @Override
            public void focusLost(final FocusEvent e) {
                comboBox.revalidate();
                comboBox.repaint();
            }
        });
        ((JComponent) comp).setBorder(null);
        return comboBoxEditor;
    }

    protected JButton createArrowButton() {
        JButton button = ArrowButton.createUpDownArrow(comboBox,
                                                       UIManager.getIcon("ComboBox.arrow.icon"),
                                                       UIManager.getIcon("ComboBox.arrowInactive.icon"),
                                                       SwingConstants.SOUTH, true, false,
                                                       new Insets(0, BUTTON_PAD, 0, BUTTON_PAD));
        button.setBorder(BorderFactory.createEmptyBorder());
        button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        return button;
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        paintBackground(g, c, c.getWidth(), c.getHeight());
        Rectangle r = rectangleForCurrentValue();
        paintCurrentValue(g, r, hasFocus);
    }

    private void paintBackground(final Graphics g, @NotNull final JComponent c, final int width, final int height) {
        final Container parent = c.getParent();
        if (parent != null && parent.isOpaque()) {
            g.setColor(parent.getBackground());
            g.fillRect(0, 0, c.getWidth(), c.getHeight());
        }
        if (comboBox.isEditable() && comboBox.getEditor() != null) {
            g.setColor(comboBox.getEditor().getEditorComponent().getBackground());
        } else {
            g.setColor(getBackground(comboBox));
        }
        if (!isTableCellEditor(c) && !isTreeCellEditor(c)) {
            DarkUIUtil.fillRoundRect((Graphics2D) g, borderSize, borderSize,
                                     width - 2 * borderSize, height - 2 * borderSize,
                                     arcSize);
        } else {
            g.fillRect(0, 0, width, height);
        }
    }

    protected Color getBackground(final JComboBox c) {
        if (!c.isEnabled()) return inactiveBackground;
        if (c.isEditable()) return editBackground;
        return background;
    }

    protected static boolean isTableCellEditor(@NotNull final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JComboBox.isTableCellEditor"));
    }

    protected static boolean isTreeCellEditor(@NotNull final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JComboBox.isTreeCellEditor"));
    }

    private Color getForeground() {
        return comboBox.isEnabled() ? comboBox.getForeground() : inactiveForeground;
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        return getMinimumSize(c);
    }

    @Override
    public Dimension getMinimumSize(final JComponent c) {
        if (!isMinimumSizeDirty) {
            return new Dimension(cachedMinimumSize);
        }
        Dimension size = getDisplaySize();
        Insets insets = getInsets();
        //calculate the width and height of the button
        int buttonHeight = size.height;
        int buttonWidth = squareButton
                          ? buttonHeight
                          : arrowButton.getPreferredSize().width
                                  + arrowButton.getInsets().left + arrowButton.getInsets().right;
        //adjust the size based on the button width
        size.height += insets.top + insets.bottom;
        size.width += insets.left + insets.right + buttonWidth;

        cachedMinimumSize.setSize(size.width, size.height);
        isMinimumSizeDirty = false;

        return new Dimension(size);
    }

    @Override
    protected Rectangle rectangleForCurrentValue() {
        Rectangle rect = super.rectangleForCurrentValue();
        if (!comboBox.getComponentOrientation().isLeftToRight()) {
            rect.x += borderSize;
            rect.width -= borderSize;
        }
        return rect;
    }

    @Override
    protected Insets getInsets() {
        if (isTableCellEditor(comboBox)) {
            return new Insets(2, 5, 2, 5);
        }
        if (comboBox.getComponentOrientation().isLeftToRight()) {
            return new InsetsUIResource(borderSize + 4, borderSize + 6,
                                        borderSize + 4, borderSize);
        } else {
            return new InsetsUIResource(borderSize + 4, borderSize,
                                        borderSize + 4, borderSize + 6);
        }
    }

    public void paintCurrentValue(final Graphics g, final Rectangle bounds, final boolean hasFocus) {
        ListCellRenderer<Object> renderer = comboBox.getRenderer();
        Component c = renderer.getListCellRendererComponent(listBox, comboBox.getSelectedItem(),
                                                            -1, false, false);
        c.setFont(comboBox.getFont());
        if (hasFocus && !isPopupVisible(comboBox)) {
            c.setForeground(listBox.getForeground());
            c.setBackground(listBox.getBackground());
        } else {
            c.setForeground(getForeground());
            c.setBackground(getBackground(comboBox));
        }

        // paint selection in table-cell-editor mode correctly
        boolean changeOpaque = isTableCellEditor(comboBox) && c.isOpaque();
        if (changeOpaque) {
            ((JComponent) c).setOpaque(false);
        }

        boolean shouldValidate = c instanceof JPanel;
        Rectangle r = new Rectangle(bounds);
        DarkUIUtil.applyInsets(r, boxPadding);
        if (isTableCellEditor(comboBox)) {
            r.x--;
        }
        currentValuePane.paintComponent(g, c, comboBox, r.x, r.y, r.width, r.height, shouldValidate);
        // return opaque for combobox popup items painting
        if (changeOpaque) {
            ((JComponent) c).setOpaque(true);
        }
    }

    @Override
    public void paintBorder(final Component c, final Graphics g2, final int x, final int y, final int width,
                            final int height) {
        if (comboBox == null || arrowButton == null) {
            return;
        }
        final boolean isTableCellEditor = isTableCellEditor(comboBox);
        final boolean isTreeCellEditor = isTreeCellEditor(comboBox);
        int bSize = !isTableCellEditor && !isTreeCellEditor ? borderSize : 0;
        int arc = arcSize;
        checkFocus();
        Graphics2D g = (Graphics2D) g2;
        GraphicsContext config = new GraphicsContext(g);
        g.translate(x, y);

        if (comboBox.isEditable()) {
            Rectangle arrowBounds = arrowButton.getBounds();
            boolean leftToRight = comboBox.getComponentOrientation().isLeftToRight();
            int off = leftToRight ? arrowBounds.x : arrowBounds.x + arrowBounds.width;
            Area rect;
            Area iconRect;
            if (!isTableCellEditor && !isTreeCellEditor) {
                rect = new Area(new RoundRectangle2D.Double(bSize, bSize, width - 2 * bSize,
                                                            height - 2 * bSize, arc, arc));
                iconRect = new Area(new Rectangle(off, 0, width, height));
            } else {
                rect = new Area(new Rectangle(0, 0, width, height));
                iconRect = new Area(new Rectangle(off, 0, width, height));
            }
            if (leftToRight) {
                rect.intersect(iconRect);
            } else {
                rect.subtract(iconRect);
            }
            g.setPaint(getArrowBackground(comboBox));
            g.fill(rect);

            g.setColor(getBorderColor());
            g.fillRect(off, bSize, 1, height - 2 * bSize);
        }

        if (!isTableCellEditor && !isTreeCellEditor) {
            if (hasFocus) {
                DarkUIUtil.paintFocusBorder(g, width, height, arcSize, borderSize);
                g.setColor(focusBorderColor);
            } else {
                g.setColor(getBorderColor());
            }
            DarkUIUtil.paintLineBorder(g, bSize, bSize, width - 2 * bSize,
                                       height - 2 * bSize, arc, true);
        } else {
            g.setColor(getBorderColor());
            Component parent = c.getParent();
            if (isTableCellEditor(c) && parent instanceof JTable) {
                JTable table = ((JTable) parent);
                if (!table.getShowHorizontalLines()) {
                    g.fillRect(0, 0, width, 1);
                    g.fillRect(0, height - 1, width, 1);
                }
                if (!table.getShowVerticalLines()) {
                    g.fillRect(0, 0, 1, height);
                    g.fillRect(width - 1, 0, 1, height);
                }
            } else {
                DarkUIUtil.drawRect(g, 0, 0, width, height, 1);
            }
        }

        g.translate(-x, -y);
        config.restore();
    }

    private void checkFocus() {
        hasFocus = DarkUIUtil.hasFocus(comboBox);
        if (hasFocus) {
            return;
        }

        final ComboBoxEditor ed = comboBox.getEditor();
        editor = ed == null ? null : ed.getEditorComponent();
        if (editor != null) {
            hasFocus = DarkUIUtil.hasFocus(editor);
        }
    }

    protected Paint getArrowBackground(final JComboBox c) {
        if (!c.isEnabled()) return inactiveBackground;
        if (c.isEditable()) {
            return new GradientPaint(0, borderSize, arrowBackgroundStart,
                                     0, c.getHeight() - borderSize, arrowBackgroundEnd);
        }
        return background;
    }

    private Color getBorderColor() {
        return comboBox.isEnabled() ? borderColor : inactiveBorderColor;
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (isTableCellEditor(c) || isTreeCellEditor(c)) {
            return new InsetsUIResource(0, 0, 0, 0);
        }
        return new InsetsUIResource(borderSize, borderSize, borderSize, borderSize);
    }

    @Override
    public boolean isBorderOpaque() {
        return !isTableCellEditor(comboBox);
    }

    public void resetPopup() {
        ((DarkComboPopup) popup).reset();
    }
}
