/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.ui.combobox;

import java.awt.*;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.LineBorder;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicComboBoxUI;
import javax.swing.plaf.basic.ComboPopup;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.components.ArrowButton;
import com.github.weisj.darklaf.ui.DividedWidgetPainter;
import com.github.weisj.darklaf.ui.list.DarkDefaultListCellRenderer;
import com.github.weisj.darklaf.ui.popupmenu.DarkPopupMenuUI;
import com.github.weisj.darklaf.ui.text.DarkTextUI;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

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
    protected Color foreground;
    protected Color inactiveForeground;
    protected Color arrowBackground;
    private Insets buttonPad;
    private Insets valueInsets;
    private Insets editorCellInsets;


    public static ComponentUI createUI(final JComponent c) {
        return new DarkComboBoxUI();
    }

    protected Component getEditorComponent() {
        ComboBoxEditor editor = comboBox.getEditor();
        if (editor == null) return null;
        return editor.getEditorComponent();
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        LookAndFeel.installProperty(comboBox, PropertyKey.OPAQUE, false);
        comboBox.putClientProperty(DarkPopupMenuUI.KEY_CONSUME_EVENT_ON_CLOSE, true);
        installBorder(comboBox);
        arcSize = UIManager.getInt("ComboBox.arc");
        borderSize = UIManager.getInt("ComboBox.borderThickness");
        background = UIManager.getColor("ComboBox.activeBackground");
        foreground = UIManager.getColor("ComboBox.foreground");
        editBackground = UIManager.getColor("ComboBox.editBackground");
        inactiveBackground = UIManager.getColor("ComboBox.inactiveBackground");
        inactiveForeground = UIManager.getColor("ComboBox.disabledForeground");
        arrowBackground = UIManager.getColor("ComboBox.arrowBackground");
        buttonPad = UIManager.getInsets("ComboBox.buttonInsets");
        valueInsets = UIManager.getInsets("ComboBox.valueInsets");
        editorCellInsets = UIManager.getInsets("ComboBox.cellEditorInsets");
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
        return new DarkComboBoxListener(this, comboBox);
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

    protected Insets getEditorInsets(final JComponent c) {
        if (ComboBoxConstants.isTreeOrTableCellEditor(c)) {
            return editorCellInsets;
        } else {
            return valueInsets;
        }
    }

    @Override
    protected ComboPopup createPopup() {
        return new DarkComboPopup(comboBox);
    }

    @Override
    protected ListCellRenderer<Object> createRenderer() {
        return new DarkDefaultListCellRenderer();
    }

    @Override
    protected ComboBoxEditor createEditor() {
        final ComboBoxEditor comboBoxEditor = super.createEditor();
        Component comp = comboBoxEditor.getEditorComponent();
        if (comp instanceof JTextField) {
            ((JTextField) comp).setColumns(0);
            ((JTextField) comp).setBorder(new LineBorder(Color.RED));
        }
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
        JButton button = ArrowButton.createUpDownArrow(comboBox,
                new ComboIcon(comboBox, UIManager.getIcon("ComboBox.arrowEditable.icon"),
                        UIManager.getIcon("ComboBox.arrow.icon")),
                UIManager.getIcon("ComboBox.arrowInactive.icon"), SwingConstants.SOUTH, true, false, buttonPad);
        button.setBorder(BorderFactory.createEmptyBorder());
        button.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        button.putClientProperty(DarkPopupMenuUI.KEY_CONSUME_EVENT_ON_CLOSE, true);
        return button;
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        if (!comboBox.isEditable()) {
            Component currentValueRenderer = getRendererForCurrentValue();
            paintBackground(g, c, currentValueRenderer);
            paintCurrentValue(g, rectangleForCurrentValue(), hasFocus, currentValueRenderer);
        } else {
            paintBackground(g, c, getEditorComponent());
        }
    }

    private void paintBackground(final Graphics g, final JComponent c, final Component currentValueRenderer) {
        final Container parent = c.getParent();
        if (parent != null && parent.isOpaque() && !c.isEnabled()) {
            g.setColor(parent.getBackground());
            g.fillRect(0, 0, c.getWidth(), c.getHeight());
            return;
        }
        boolean isCellEditor = ComboBoxConstants.isTreeOrTableCellEditor(c);
        Color rendererBg = currentValueRenderer != null ? currentValueRenderer.getBackground() : null;
        Color bg = PropertyUtil.chooseColor(rendererBg, getBackground(comboBox));

        Color splitBg = getArrowBackground(comboBox);
        Rectangle arrowRect = comboBox.isEditable() ? arrowButton.getBounds() : null;
        DividedWidgetPainter.paintBackground((Graphics2D) g, c, arcSize, arrowRect, bg, splitBg, isCellEditor);
    }

    protected Color getBackground(final JComponent c) {
        return c.getBackground();
    }

    protected void updateForeground(final JComboBox<?> c) {
        Color color = foreground;
        if (!c.isEnabled()) {
            color = inactiveForeground;
        }
        if (c.isEditable()) {
            PropertyUtil.installForeground(getEditorComponent(), color);
        }
        PropertyUtil.installForeground(c, color);
    }

    protected void updateBackground(final JComboBox<?> c) {
        Color color = background;
        if (!c.isEnabled()) {
            color = inactiveBackground;
        } else if (c.isEditable()) {
            color = editBackground;
        }
        if (c.isEditable()) {
            PropertyUtil.installBackground(getEditorComponent(), color);
        }
        PropertyUtil.installBackground(c, color);
    }

    protected Color getArrowBackground(final JComboBox<?> c) {
        if (!c.isEnabled()) return inactiveBackground;
        if (c.isEditable()) return arrowBackground;
        return background;
    }

    private Color getForeground(final Component c) {
        return c.getForeground();
    }

    @Override
    public Dimension getMinimumSize(final JComponent c) {
        Dimension size = super.getMinimumSize(c);
        Dimension editorSize = DarkUIUtil.getMinimumSize((JComponent) getEditorComponent());

        Insets ins = getInsets();
        DarkUIUtil.addInsets(size, ins);

        DarkUIUtil.addInsets(editorSize, ins);
        DarkUIUtil.addInsets(editorSize, getEditorInsets(comboBox));

        Insets bPad = buttonPad;
        Dimension abSize = arrowButton.getPreferredSize();
        size.width += bPad.left + bPad.right;
        editorSize.width += abSize.width;

        int width = Math.max(editorSize.width, size.width);
        int height = Math.max(editorSize.height, Math.max(abSize.height, size.height));

        return new Dimension(width, height);
    }

    @Override
    protected Rectangle rectangleForCurrentValue() {
        Rectangle rect = new Rectangle(comboBox.getSize());
        Insets i = getInsets();
        DarkUIUtil.applyInsets(rect, i);
        Insets pad = getEditorInsets(comboBox);
        DarkUIUtil.applyInsets(rect, pad);

        boolean ltr = comboBox.getComponentOrientation().isLeftToRight();
        int extra = ltr ? pad.right : pad.left;
        rect.width += extra;
        if (!ltr) rect.x -= extra;

        if (arrowButton != null) {
            int bw = arrowButton.getWidth();
            rect.width -= bw;
            if (!ltr) {
                rect.x += bw;
            }
        }
        if (!ltr && comboBox.isEditable()) {
            // Compensate for the width of the caret which will shift the contained text.
            Component editorComp = getEditorComponent();
            if (editorComp instanceof JTextComponent) {
                int caretWidth = DarkTextUI.getCaretWidth((JTextComponent) editorComp);
                rect.width += caretWidth;
            }
        }

        return rect;
    }

    @SuppressWarnings("unchecked")
    protected Component getRendererForCurrentValue() {
        return comboBox.getRenderer().getListCellRendererComponent(listBox, comboBox.getSelectedItem(),
                comboBox.getSelectedIndex(), false, false);
    }

    public void paintCurrentValue(final Graphics g, final Rectangle bounds, final boolean hasFocus, final Component c) {
        PropertyUtil.installFont(c, comboBox.getFont());
        c.setFont(comboBox.getFont());
        if (hasFocus && !isPopupVisible(comboBox)) {
            PropertyUtil.installForeground(c, listBox.getForeground());
            PropertyUtil.installBackground(c, listBox.getBackground());
        } else {
            PropertyUtil.installForeground(c, getForeground(comboBox));
            PropertyUtil.installBackground(c, getBackground(comboBox));
        }
        if (c instanceof JComponent) {
            PropertyUtil.installBorder((JComponent) c, null);
        }

        // paint selection in table-cell-editor mode correctly
        boolean changeOpaque = c instanceof JComponent && c.isOpaque()
                && (!comboBox.isEnabled() || ComboBoxConstants.isTreeOrTableCellEditor(comboBox));
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

        editor = getEditorComponent();
        if (editor != null) {
            hasFocus = DarkUIUtil.hasFocus(editor);
        }
    }

    public void resetPopup() {
        ((DarkComboPopup) popup).reset();
    }
}
