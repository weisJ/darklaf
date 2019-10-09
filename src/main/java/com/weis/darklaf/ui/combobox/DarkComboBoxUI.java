package com.weis.darklaf.ui.combobox;

import com.weis.darklaf.components.ArrowButton;
import com.weis.darklaf.ui.text.DarkTextBorder;
import com.weis.darklaf.ui.text.DarkTextFieldUI;
import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.GraphicsContext;
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

public class DarkComboBoxUI extends BasicComboBoxUI implements Border {

    private static final int BUTTON_PAD = 7;
    private static final int BORDER_SIZE = DarkTextBorder.BORDER_SIZE;
    private static final int ARC_SIZE = DarkTextFieldUI.SEARCH_ARC_SIZE;

    private final MouseListener mouseListener = new MouseAdapter() {
        @Override
        public void mousePressed(final MouseEvent e) {
            super.mousePressed(e);
            comboBox.getEditor().getEditorComponent().requestFocus();
        }
    };
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
    protected ComboBoxEditor createEditor() {
        final ComboBoxEditor comboBoxEditor = super.createEditor();
        var comp = comboBoxEditor.getEditorComponent();
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
        JButton button = ArrowButton.createUpDownArrow(comboBox, SwingConstants.SOUTH, true, false,
                                                       new Insets(0, BUTTON_PAD, 0, BUTTON_PAD));
        button.setBorder(BorderFactory.createEmptyBorder());
        return button;
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        paintBackground(g, c, c.getWidth(), c.getHeight());
        Rectangle r = rectangleForCurrentValue();
        if (!isTableCellEditor(c)) {
            checkFocus();
            paintCurrentValueBackground(g, r, hasFocus);
        }
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
            g.setColor(getBackground());
        }
        if (!isTableCellEditor(c) && !isTreeCellEditor(c)) {
            g.fillRoundRect(BORDER_SIZE, BORDER_SIZE,
                            width - 2 * BORDER_SIZE, height - 2 * BORDER_SIZE,
                            ARC_SIZE, ARC_SIZE);
        } else {
            g.fillRect(0, 0, width, height);
        }
    }

    protected static boolean isTableCellEditor(@NotNull final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JComboBox.isTableCellEditor"));
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

    private Color getBackground() {
        return comboBox.isEnabled() ? UIManager.getColor("ComboBox.activeBackground")
                                    : UIManager.getColor("ComboBox.inactiveBackground");
    }

    protected static boolean isTreeCellEditor(@NotNull final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JComboBox.isTreeCellEditor"));
    }

    private Color getForeground() {
        return comboBox.isEnabled() ? comboBox.getForeground()
                                    : UIManager.getColor("ComboBox.disabledForeground");
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
    public Dimension getPreferredSize(final JComponent c) {
        return getMinimumSize(c);
    }

    @Override
    protected Rectangle rectangleForCurrentValue() {
        var rect = super.rectangleForCurrentValue();
        if (!comboBox.getComponentOrientation().isLeftToRight()) {
            rect.x += BORDER_SIZE;
            rect.width -= BORDER_SIZE;
        }
        return rect;
    }

    @Override
    protected Insets getInsets() {
        if (isTableCellEditor(comboBox)) {
            return new Insets(2, 5, 2, 5);
        }
        if (comboBox.getComponentOrientation().isLeftToRight()) {
            return new InsetsUIResource(BORDER_SIZE + 4, BORDER_SIZE + 6,
                                        BORDER_SIZE + 4, BORDER_SIZE);
        } else {
            return new InsetsUIResource(BORDER_SIZE + 4, BORDER_SIZE,
                                        BORDER_SIZE + 4, BORDER_SIZE + 6);
        }
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (isTableCellEditor(c) || isTreeCellEditor(c)) {
            return new InsetsUIResource(0, 0, 0, 0);
        }
        return new InsetsUIResource(BORDER_SIZE, BORDER_SIZE, BORDER_SIZE, BORDER_SIZE);
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
            c.setBackground(getBackground());
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
        int bSize = !isTableCellEditor && !isTreeCellEditor ? BORDER_SIZE : 0;
        int arc = ARC_SIZE;
        checkFocus();
        Graphics2D g = (Graphics2D) g2;
        GraphicsContext config = new GraphicsContext(g);
        g.translate(x, y);

        if (comboBox.isEditable()) {
            var arrowBounds = arrowButton.getBounds();
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
            g.setColor(getBackground());
            g.fill(rect);

            g.setColor(getBorderColor());
            g.fillRect(off, bSize, 1, height - 2 * bSize);
        }

        if (!isTableCellEditor && !isTreeCellEditor) {
            if (hasFocus) {
                DarkUIUtil.paintFocusBorder(g, width, height, ARC_SIZE, true);
                g.setColor(UIManager.getColor("ComboBox.focusBorderColor"));
            } else {
                g.setColor(getBorderColor());
            }
            DarkUIUtil.paintLineBorder(g, bSize, bSize, width - 2 * bSize,
                                       height - 2 * bSize, arc, true);
        } else {
            g.setColor(getBorderColor());
            var parent = c.getParent();
            if (isTableCellEditor(c) && parent instanceof JTable) {
                var table = ((JTable) parent);
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

    private Color getBorderColor() {
        return comboBox.isEnabled() ? UIManager.getColor("ComboBox.activeBorderColor")
                                    : UIManager.getColor("ComboBox.inactiveBorderColor");
    }

    @Override
    public boolean isBorderOpaque() {
        return !isTableCellEditor(comboBox);
    }

    public void resetPopup() {
        ((DarkComboPopup) popup).reset();
    }

}
