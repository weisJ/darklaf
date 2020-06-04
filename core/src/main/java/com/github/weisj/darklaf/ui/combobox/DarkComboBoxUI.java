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
 *
 */
package com.github.weisj.darklaf.ui.combobox;

import java.awt.*;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicComboBoxUI;
import javax.swing.plaf.basic.ComboPopup;

import com.github.weisj.darklaf.components.ArrowButton;
import com.github.weisj.darklaf.delegate.LayoutManagerDelegate;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.list.DarkListCellRendererDelegate;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyKey;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkComboBoxUI extends BasicComboBoxUI implements ComboBoxConstants {

    protected DarkComboBoxListener comboBoxListener;

    protected int arcSize;
    protected int borderSize;
    protected Color background;
    protected Color editBackground;
    protected Color inactiveBackground;
    protected Color inactiveForeground;
    protected Color arrowBackground;
    private Insets boxPadding;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkComboBoxUI();
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        LookAndFeel.installProperty(comboBox, PropertyKey.OPAQUE, false);
        installBorder(comboBox);
        arcSize = UIManager.getInt("ComboBox.arc");
        boxPadding = UIManager.getInsets("ComboBox.insets");
        borderSize = UIManager.getInt("ComboBox.borderThickness");
        background = UIManager.getColor("ComboBox.activeBackground");
        editBackground = UIManager.getColor("ComboBox.editBackground");
        inactiveBackground = UIManager.getColor("ComboBox.inactiveBackground");
        inactiveForeground = UIManager.getColor("ComboBox.disabledForeground");
        arrowBackground = UIManager.getColor("ComboBox.arrowBackground");
        if (boxPadding == null) boxPadding = new Insets(0, 0, 0, 0);
    }

    protected void installBorder(final JComponent c) {
        Border b = c.getBorder();
        if (b == null || b instanceof UIResource) {
            c.setBorder(createBorder());
        }
    }

    protected Border createBorder() {
        return new DarkComboBoxBorder(this);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        comboBoxListener = createComboBoxListener();
        comboBox.addMouseListener(comboBoxListener);
        comboBox.addPropertyChangeListener(comboBoxListener);
    }

    protected DarkComboBoxListener createComboBoxListener() {
        return new DarkComboBoxListener(comboBox);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        comboBox.removeMouseListener(comboBoxListener);
        comboBox.removePropertyChangeListener(comboBoxListener);
        comboBoxListener = null;
    }

    public JComboBox<?> getComboBox() {
        return comboBox;
    }

    public AbstractButton getArrowButton() {
        return arrowButton;
    }

    public boolean getHasFocus() {
        return hasFocus;
    }

    @Override
    protected ComboPopup createPopup() {
        return new DarkComboPopup(comboBox, borderSize);
    }

    @Override
    protected ListCellRenderer<Object> createRenderer() {
        return new DarkListCellRendererDelegate();
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

            private void process(final KeyEvent e) {
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
        return comboBoxEditor;
    }

    protected JButton createArrowButton() {
        int buttonPad = UIManager.getInt("ComboBox.buttonPad");
        JButton button = ArrowButton.createUpDownArrow(comboBox,
                                                       new ComboIcon(comboBox,
                                                                     UIManager.getIcon("ComboBox.arrowEditable.icon"),
                                                                     UIManager.getIcon("ComboBox.arrow.icon")),
                                                       UIManager.getIcon("ComboBox.arrowInactive.icon"),
                                                       SwingConstants.SOUTH, true, false,
                                                       new Insets(0, buttonPad, 0, buttonPad));
        button.setBorder(BorderFactory.createEmptyBorder());
        button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        return button;
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        paintBackground(g, c, c.getWidth(), c.getHeight());
        Rectangle r = rectangleForCurrentValue();
        if (!comboBox.isEditable()) {
            paintCurrentValue(g, r, hasFocus);
        }
    }

    private void paintBackground(final Graphics g, final JComponent c, final int width, final int height) {
        final Container parent = c.getParent();
        if (parent != null && parent.isOpaque() && !c.isEnabled()) {
            g.setColor(parent.getBackground());
            g.fillRect(0, 0, c.getWidth(), c.getHeight());
            return;
        }

        boolean isCellEditor = ComboBoxConstants.isTreeOrTableCellEditor(c);

        if (comboBox.isEditable() && comboBox.getEditor() != null) {
            Component editorComp = comboBox.getEditor().getEditorComponent();
            if (comboBox.isEnabled()) {
                g.setColor(editorComp.getBackground());
            } else {
                g.setColor(inactiveBackground);
            }
        } else {
            g.setColor(getBackground(comboBox));
        }
        if (!isCellEditor) {
            PaintUtil.fillRoundRect((Graphics2D) g, borderSize, borderSize,
                                    width - 2 * borderSize, height - 2 * borderSize, arcSize);
        } else {
            g.fillRect(0, 0, width, height);
        }
        if (comboBox.isEditable()) {
            int bSize = !isCellEditor ? borderSize : 0;
            paintArrowBackground(width, height, comboBox, arrowButton, isCellEditor,
                                 bSize, arcSize, (Graphics2D) g);
        }
    }

    public void paintArrowBackground(final int width, final int height, final JComboBox<?> comboBox,
                                     final AbstractButton arrowButton, final boolean isCellEditor,
                                     final int bSize, final int arc,
                                     final Graphics2D g) {
        Rectangle arrowBounds = arrowButton.getBounds();
        boolean leftToRight = comboBox.getComponentOrientation().isLeftToRight();

        Shape oldClip = g.getClip();
        g.setColor(getArrowBackground(comboBox));

        if (leftToRight) {
            g.clipRect(arrowBounds.x, 0, width - arrowBounds.x, height);
        } else {
            g.clipRect(0, 0, arrowBounds.x + arrowBounds.width, height);
        }
        if (isCellEditor) {
            g.fillRect(0, 0, width, height);
        } else {
            PaintUtil.fillRoundRect(g, bSize, bSize, width - 2 * bSize, height - 2 * bSize,
                                    arc, false);
        }
        g.setClip(oldClip);
    }

    protected Color getBackground(final JComboBox<?> c) {
        if (!c.isEnabled()) return inactiveBackground;
        if (c.isEditable()) return editBackground;
        return background;
    }

    protected Color getArrowBackground(final JComboBox<?> c) {
        if (!c.isEnabled()) return inactiveBackground;
        if (c.isEditable()) return arrowBackground;
        return background;
    }

    @Override
    protected LayoutManager createLayoutManager() {
        return new LayoutManagerDelegate(super.createLayoutManager()) {

            @Override
            public void layoutContainer(final Container parent) {
                super.layoutContainer(parent);
                if (ComboBoxConstants.isTreeOrTableCellEditor(comboBox)) {
                    int adj = borderSize / 2;
                    if (!comboBox.getComponentOrientation().isLeftToRight()) adj *= -1;
                    Rectangle bounds = arrowButton.getBounds();
                    bounds.x += adj;
                    arrowButton.setBounds(bounds);
                }
            }
        };
    }

    private Color getForeground(final Component c) {
        return c.isEnabled() ? c.getForeground() : inactiveForeground;
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
        // calculate the width and height of the button
        int buttonHeight = size.height;
        int buttonWidth = squareButton
                ? buttonHeight
                : arrowButton.getPreferredSize().width
                  + arrowButton.getInsets().left + arrowButton.getInsets().right;
        // adjust the size based on the button width
        size.height += insets.top + insets.bottom;
        size.width += insets.left + insets.right + buttonWidth;

        cachedMinimumSize.setSize(size.width, size.height);
        isMinimumSizeDirty = false;

        return new Dimension(size);
    }

    @Override
    protected Rectangle rectangleForCurrentValue() {
        Rectangle rect = super.rectangleForCurrentValue();
        if (comboBox.isEditable()) {
            if (comboBox.getComponentOrientation().isLeftToRight()) {
                rect.x += boxPadding.left;
                rect.width -= boxPadding.left;
            } else {
                rect.width -= boxPadding.right - borderSize;
            }
        }
        return rect;
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
            c.setForeground(getForeground(comboBox));
            c.setBackground(getBackground(comboBox));
        }

        // paint selection in table-cell-editor mode correctly
        boolean changeOpaque = c.isOpaque() && (!comboBox.isEnabled()
                                                || ComboBoxConstants.isTreeOrTableCellEditor(comboBox));
        if (changeOpaque) {
            ((JComponent) c).setOpaque(false);
        }

        boolean shouldValidate = c instanceof JPanel;
        Rectangle r = new Rectangle(bounds);
        currentValuePane.paintComponent(g, c, comboBox, r.x, r.y, r.width, r.height, shouldValidate);
        // return opaque for combobox popup items painting
        if (changeOpaque) {
            ((JComponent) c).setOpaque(true);
        }
    }

    public void checkFocus() {
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

    public void resetPopup() {
        ((DarkComboPopup) popup).reset();
    }
}
