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
package com.github.weisj.darklaf.ui.spinner;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicSpinnerUI;

import com.github.weisj.darklaf.components.ArrowButton;
import com.github.weisj.darklaf.delegate.LayoutManagerDelegate;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.util.PropertyKey;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkSpinnerUI extends BasicSpinnerUI implements SpinnerConstants {

    protected DarkSpinnerListener spinnerListener;

    protected Color arrowBackground;
    protected Color background;
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
    private JButton prevButton;
    private Component editorComponent;
    private JButton nextButton;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkSpinnerUI();
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        arc = UIManager.getInt("Spinner.arc");
        borderSize = UIManager.getInt("Spinner.borderThickness");
        background = UIManager.getColor("Spinner.activeBackground");
        inactiveBackground = UIManager.getColor("Spinner.inactiveBackground");
        arrowBackground = UIManager.getColor("Spinner.arrowBackground");

        arrowDownIcon = UIManager.getIcon("Spinner.arrowDown.icon");
        arrowUpIcon = UIManager.getIcon("Spinner.arrowUp.icon");
        minusIcon = UIManager.getIcon("Spinner.minus.icon");
        plusIcon = UIManager.getIcon("Spinner.plus.icon");

        arrowDownInactiveIcon = UIManager.getIcon("Spinner.arrowDownInactive.icon");
        arrowUpInactiveIcon = UIManager.getIcon("Spinner.arrowUpInactive.icon");
        minusInactiveIcon = UIManager.getIcon("Spinner.minusInactive.icon");
        plusInactiveIcon = UIManager.getIcon("Spinner.plusInactive.icon");
        LookAndFeel.installProperty(spinner, PropertyKey.OPAQUE, false);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        spinnerListener = createSpinnerListener();
        spinner.addMouseListener(spinnerListener);
        spinner.addPropertyChangeListener(spinnerListener);
    }

    protected DarkSpinnerListener createSpinnerListener() {
        return new DarkSpinnerListener(spinner, this);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        if (editor != null && editor.getComponents().length > 0) {
            editor.getComponents()[0].removeFocusListener(spinnerListener);
        }
        spinner.removeMouseListener(spinnerListener);
        spinner.removePropertyChangeListener(spinnerListener);
        spinnerListener = null;
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

            protected void adjustButton(final JComponent button, final int adj) {
                Rectangle bounds = button.getBounds();
                bounds.x += adj;
                button.setBounds(bounds);
            }

            @Override
            public void layoutContainer(final Container parent) {
                super.layoutContainer(parent);
                if (SpinnerConstants.isTreeOrTableCellEditor(spinner)) {
                    int adj = borderSize / 2;
                    if (!spinner.getComponentOrientation().isLeftToRight()) adj *= -1;
                    adjustButton(prevButton, adj);
                    adjustButton(nextButton, adj);
                }
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
        nextButton = createArrow(SwingConstants.NORTH);
        nextButton.setName("Spinner.nextButton");
        nextButton.setBorder(new EmptyBorder(1, 1, 1, 1));
        nextButton.setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR));
        installNextButtonListeners(nextButton);
        return nextButton;
    }

    @Override
    protected JComponent createEditor() {
        editor = super.createEditor();
        if (editor instanceof JSpinner.DefaultEditor) {
            editorComponent = ((JSpinner.DefaultEditor) editor).getTextField();
        } else {
            editorComponent = editor;
        }
        editorComponent.addFocusListener(spinnerListener);
        return editor;
    }

    @Override
    protected void replaceEditor(final JComponent oldEditor, final JComponent newEditor) {
        super.replaceEditor(oldEditor, newEditor);
        editor = newEditor;
        if (oldEditor != null && oldEditor.getComponents().length > 0) {
            oldEditor.getComponents()[0].removeFocusListener(spinnerListener);
        }
        if (newEditor != null && newEditor.getComponents().length > 0) {
            Component comp = newEditor.getComponents()[0];
            comp.addFocusListener(spinnerListener);
        }
    }

    private JButton createArrow(final int direction) {
        int buttonPad = UIManager.getInt("Spinner.buttonPad");
        Insets insets = new Insets(0, buttonPad, 0, buttonPad);
        JButton button = ArrowButton.createUpDownArrow(spinner, getArrowIcon(direction),
                getArrowInactiveIcon(direction), direction, false, true, insets);
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
        final Container parent = c.getParent();
        if (parent != null && parent.isOpaque() && !c.isEnabled()) {
            g.setColor(parent.getBackground());
            g.fillRect(0, 0, c.getWidth(), c.getHeight());
            return;
        }

        int size = borderSize;
        int width = c.getWidth();
        int height = c.getHeight();
        JComponent editor = spinner.getEditor();

        if (editorComponent != null) {
            editorComponent.setBackground(getBackground(c));
            g.setColor(editorComponent.getBackground());
        } else {
            g.setColor(getBackground(c));
        }
        if (!SpinnerConstants.isTreeOrTableCellEditor(c)) {
            PaintUtil.fillRoundRect((Graphics2D) g, size, size, width - 2 * size, height - 2 * size, arc);
        } else {
            g.fillRect(0, 0, width, height);
        }
        if (editor != null) {
            paintSpinBackground((Graphics2D) g, width, height, size, arc);
        }
    }

    protected void paintSpinBackground(final Graphics2D g, final int width, final int height, final int bSize,
            final int arc) {
        Rectangle arrowBounds = prevButton.getBounds();
        boolean leftToRight = spinner.getComponentOrientation().isLeftToRight();

        Shape oldClip = g.getClip();
        g.setColor(getArrowBackground(spinner));

        if (leftToRight) {
            g.clipRect(arrowBounds.x, 0, width - arrowBounds.x, height);
        } else {
            g.clipRect(0, 0, arrowBounds.x + arrowBounds.width, height);
        }
        if (SpinnerConstants.isTreeOrTableCellEditor(spinner)) {
            g.fillRect(0, 0, width, height);
        } else {
            PaintUtil.fillRoundRect(g, bSize, bSize, width - 2 * bSize, height - 2 * bSize, arc);
        }
        g.setClip(oldClip);
    }

    protected Color getBackground(final JComponent c) {
        return c == null ? inactiveBackground : c.getBackground();
    }

    protected Color getArrowBackground(final JComponent c) {
        return c == null || !c.isEnabled() ? inactiveBackground : arrowBackground;
    }

    public Component getEditorComponent() {
        return editorComponent;
    }

    public JComponent getEditor() {
        return editor;
    }
}
