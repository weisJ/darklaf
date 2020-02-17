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
package com.github.weisj.darklaf.ui.spinner;

import com.github.weisj.darklaf.components.ArrowButton;
import com.github.weisj.darklaf.decorators.LayoutManagerDelegate;
import com.github.weisj.darklaf.util.DarkUIUtil;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicSpinnerUI;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.Area;
import java.awt.geom.RoundRectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkSpinnerUI extends BasicSpinnerUI implements PropertyChangeListener {

    private final FocusListener focusListener = new FocusAdapter() {
        @Override
        public void focusGained(final FocusEvent e) {
            spinner.repaint();
        }

        @Override
        public void focusLost(final FocusEvent e) {
            spinner.repaint();
        }
    };
    private final MouseListener mouseListener = new MouseAdapter() {
        @Override
        public void mousePressed(final MouseEvent e) {
            super.mousePressed(e);
            spinner.getEditor().requestFocus();
        }
    };
    protected Color arrowBackgroundStart;
    protected Color background;
    protected Color arrowBackgroundEnd;
    protected Icon arrowDownIcon;
    protected Color inactiveBackground;
    protected int arc;
    protected int borderSize;
    protected Icon arrowUpIcon;
    protected Icon plusIcon;
    protected Icon minusIcon;
    protected Icon arrowDownInactiveIcon;
    protected Icon arrowUpInactiveIcon;
    protected Icon plusInactiveIcon;
    protected Icon minusInactiveIcon;
    private JComponent editor;
    private Color compColor;
    private JButton prevButton;
    private Component editorComponent;


    public static ComponentUI createUI(final JComponent c) {
        return new DarkSpinnerUI();
    }

    protected static boolean usePlusMinusIcons(final JSpinner spinner) {
        return "plusMinus".equals(spinner.getClientProperty("JSpinner.variant"));
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        spinner.addMouseListener(mouseListener);
        spinner.addPropertyChangeListener(this);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        spinner.removeMouseListener(mouseListener);
        spinner.removePropertyChangeListener(this);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        arc = UIManager.getInt("Spinner.arc");
        borderSize = UIManager.getInt("Spinner.borderThickness");
        background = UIManager.getColor("Spinner.activeBackground");
        inactiveBackground = UIManager.getColor("Spinner.inactiveBackground");
        arrowBackgroundStart = UIManager.getColor("Spinner.arrowBackgroundStart");
        arrowBackgroundEnd = UIManager.getColor("Spinner.arrowBackgroundEnd");

        arrowDownIcon = UIManager.getIcon("Spinner.arrowDown.icon");
        arrowUpIcon = UIManager.getIcon("Spinner.arrowUp.icon");
        minusIcon = UIManager.getIcon("Spinner.minus.icon");
        plusIcon = UIManager.getIcon("Spinner.plus.icon");

        arrowDownInactiveIcon = UIManager.getIcon("Spinner.arrowDownInactive.icon");
        arrowUpInactiveIcon = UIManager.getIcon("Spinner.arrowUpInactive.icon");
        minusInactiveIcon = UIManager.getIcon("Spinner.minusInactive.icon");
        plusInactiveIcon = UIManager.getIcon("Spinner.plusInactive.icon");
        LookAndFeel.installProperty(spinner, "opaque", false);
    }

    protected LayoutManager createLayout() {
        return new LayoutManagerDelegate(super.createLayout()) {
            private Component editor = null;

            @Override
            public void addLayoutComponent(final String name, final Component comp) {
                super.addLayoutComponent(name, comp);
                if ("Editor".equals(name)) {
                    editor = comp;
                }
            }

            @Override
            public void layoutContainer(final Container parent) {
                super.layoutContainer(parent);
                if (editor != null && !spinner.getComponentOrientation().isLeftToRight()) {
                    Rectangle bounds = editor.getBounds();
                    bounds.x += borderSize;
                    bounds.width -= borderSize;
                    editor.setBounds(bounds);
                }
            }
        };
    }

    @Override
    protected Component createPreviousButton() {
        prevButton = createArrow(SwingConstants.SOUTH);
        prevButton.setName("Spinner.previousButton");
        prevButton.setBorder(new EmptyBorder(1, 1, 1, 1));
        prevButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        installPreviousButtonListeners(prevButton);
        return prevButton;
    }

    @Override
    protected Component createNextButton() {
        JButton nextButton = createArrow(SwingConstants.NORTH);
        nextButton.setName("Spinner.nextButton");
        nextButton.setBorder(new EmptyBorder(1, 1, 1, 1));
        nextButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        installNextButtonListeners(nextButton);
        return nextButton;
    }

    @Override
    protected JComponent createEditor() {
        editor = super.createEditor();
        editorComponent = ((JSpinner.DefaultEditor) editor).getTextField();
        editorComponent.addFocusListener(focusListener);
        return editor;
    }

    @SuppressWarnings("ConstantConditions")
    @Override
    protected void replaceEditor(final JComponent oldEditor, final JComponent newEditor) {
        super.replaceEditor(oldEditor, newEditor);
        editor = newEditor;
        if (oldEditor != null && oldEditor.getComponents().length > 0) {
            oldEditor.getComponents()[0].removeFocusListener(focusListener);
        }
        if (newEditor != null && newEditor.getComponents().length > 0) {
            Component comp = newEditor.getComponents()[0];
            comp.addFocusListener(focusListener);
        }
    }


    private JButton createArrow(final int direction) {
        int buttonPad = UIManager.getInt("Spinner.buttonPad");
        Insets insets = new Insets(0, buttonPad, 0, buttonPad);
        JButton button = ArrowButton.createUpDownArrow(spinner,
                                                       getArrowIcon(direction),
                                                       getArrowInactiveIcon(direction), direction,
                                                       false, true, insets);
        Border buttonBorder = UIManager.getBorder("Spinner.arrowButtonBorder");
        if (buttonBorder instanceof UIResource) {
            // Wrap the border to avoid having the UIResource be replaced by
            // the ButtonUI. This is the opposite of using BorderUIResource.
            button.setBorder(new CompoundBorder(buttonBorder, null));
        } else {
            button.setBorder(buttonBorder);
        }
        button.setInheritsPopupMenu(true);
        return button;
    }

    protected SpinnerIcon getArrowIcon(final int direction) {
        if (direction == SwingConstants.SOUTH) {
            return new SpinnerIcon(spinner, arrowDownIcon, minusIcon);
        } else {
            return new SpinnerIcon(spinner, arrowUpIcon, plusIcon);
        }
    }

    protected SpinnerIcon getArrowInactiveIcon(final int direction) {
        if (direction == SwingConstants.SOUTH) {
            return new SpinnerIcon(spinner, arrowDownInactiveIcon, minusInactiveIcon);
        } else {
            return new SpinnerIcon(spinner, arrowUpInactiveIcon, plusInactiveIcon);
        }
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        int size = borderSize;
        int width = c.getWidth();
        int height = c.getHeight();
        JComponent editor = spinner.getEditor();
        if (editorComponent != null) {
            editorComponent.setBackground(getBackground(c));
            g.setColor(editorComponent.getBackground());
        } else {
            ((Graphics2D) g).setPaint(getBackground(c));
        }
        if (!isTableCellEditor(c) && !isTreeCellEditor(c)) {
            DarkUIUtil.fillRoundRect((Graphics2D) g, size, size, width - 2 * size, height - 2 * size, arc);
        } else {
            Rectangle bounds = prevButton.getBounds();
            boolean leftToRight = spinner.getComponentOrientation().isLeftToRight();
            int off = leftToRight ? bounds.x + 1 : bounds.x + bounds.width;
            if (leftToRight) {
                g.fillRect(0, 0, off, height);
            } else {
                g.fillRect(off, 0, width - off, height);
            }
        }
        if (editor != null) {
            paintSpinBackground((Graphics2D) g, width, height, size, arc);
        }
    }

    protected Color getBackground(final JComponent c) {
        return c == null || !c.isEnabled() ? inactiveBackground : background;
    }

    protected static boolean isTableCellEditor(final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JSpinner.isTableCellEditor"));
    }

    protected static boolean isTreeCellEditor(final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JSpinner.isTreeCellEditor"));
    }

    private void paintSpinBackground(final Graphics2D g, final int width, final int height,
                                     final int bSize, final int arc) {
        Rectangle bounds = prevButton.getBounds();
        boolean leftToRight = spinner.getComponentOrientation().isLeftToRight();
        int off = leftToRight ? bounds.x : bounds.x + bounds.width;
        Area rect = new Area(new RoundRectangle2D.Double(bSize - 1, bSize - 1, width - 2 * bSize + 2, height - 2 * bSize + 1,
                                                         arc, arc));
        Area iconRect = new Area(new Rectangle(off, 0, width, height));
        if (leftToRight) {
            rect.intersect(iconRect);
        } else {
            rect.subtract(iconRect);
        }
        g.setPaint(getArrowBackground(spinner));
        g.fill(rect);
    }

    protected Paint getArrowBackground(final JComponent c) {
        return c == null || !c.isEnabled() ? inactiveBackground
                                           : new GradientPaint(0, borderSize, arrowBackgroundStart,
                                                               0, c.getHeight() - borderSize, arrowBackgroundEnd);
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if ("opaque".equals(key)) {
            boolean val = Boolean.TRUE.equals(evt.getNewValue());
            spinner.getEditor().setOpaque(val);
            if (editorComponent instanceof JComponent) {
                ((JComponent) editorComponent).setOpaque(val);
            }
        } else if ("JSpinner.isTableCellEditor".equals(key)) {
            if (Boolean.FALSE.equals(evt.getNewValue())) {
                if (editor instanceof JSpinner.DefaultEditor) {
                    // if editor alignment isn't set in LAF, we get 0 (CENTER) here
                    int alignment = UIManager.getInt("Spinner.editorAlignment");
                    JTextField text = ((JSpinner.DefaultEditor) editor).getTextField();
                    text.setHorizontalAlignment(alignment);
                }
            }
            spinner.repaint();
        } else if ("JSpinner.cellEditorAlignment".equals(key) && isTableCellEditor(spinner)) {
            if (editorComponent instanceof JTextField && evt.getNewValue() instanceof Integer) {
                ((JTextField) editorComponent).setHorizontalAlignment((Integer) evt.getNewValue());
            }
            spinner.repaint();
        } else if ("JSpinner.variant".equals(key)) {
            spinner.repaint();
        } else if ("JSpinner.isTreeCellEditor".equals(key)) {
            spinner.repaint();
        }
    }

    protected static class SpinnerIcon implements Icon {

        private final JSpinner spinner;
        private final Icon icon;
        private final Icon mathIcon;


        public SpinnerIcon(final JSpinner spinner, final Icon icon, final Icon mathIcon) {
            this.spinner = spinner;
            this.icon = icon;
            this.mathIcon = mathIcon;
        }

        @Override
        public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
            getCurrent().paintIcon(c, g, x, y);
        }

        protected Icon getCurrent() {
            return usePlusMinusIcons(spinner) ? mathIcon : icon;
        }

        @Override
        public int getIconWidth() {
            return getCurrent().getIconWidth();
        }

        @Override
        public int getIconHeight() {
            return getCurrent().getIconHeight();
        }
    }
}
