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
package com.github.weisj.darklaf.ui.spinner;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicSpinnerUI;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.components.ArrowButton;
import com.github.weisj.darklaf.ui.DividedWidgetPainter;
import com.github.weisj.darklaf.ui.VisualPaddingListener;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

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
    protected Icon arrowUpIcon;
    protected Icon plusIcon;
    protected Icon minusIcon;
    protected Icon arrowDownInactiveIcon;
    protected Icon arrowUpInactiveIcon;
    protected Icon plusInactiveIcon;
    protected Icon minusInactiveIcon;
    private JComponent editor;
    private JButton prevButton;
    private JComponent editorComponent;
    private VisualPaddingListener visualPaddingListener;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkSpinnerUI();
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        arc = UIManager.getInt("Spinner.arc");
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
        updateBackground();
        LookAndFeel.installProperty(spinner, PropertyKey.OPAQUE, false);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        spinnerListener = createSpinnerListener();
        spinner.addMouseListener(spinnerListener);
        spinner.addMouseWheelListener(spinnerListener);
        spinner.addPropertyChangeListener(spinnerListener);
        visualPaddingListener = new VisualPaddingListener();
        spinner.addPropertyChangeListener(visualPaddingListener);
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
        spinner.removeMouseWheelListener(spinnerListener);
        spinner.removePropertyChangeListener(spinnerListener);
        spinnerListener = null;
        spinner.removePropertyChangeListener(visualPaddingListener);
        visualPaddingListener = null;
    }

    protected LayoutManager createLayout() {
        return new DarkSpinnerLayout();
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
        updateEditorComponent();
        return editor;
    }

    private void updateEditorComponent() {
        if (editor instanceof JSpinner.DefaultEditor) {
            editorComponent = ((JSpinner.DefaultEditor) editor).getTextField();
        } else {
            editorComponent = editor;
        }
        if (editorComponent instanceof JTextComponent) {
            ((JTextComponent) editorComponent).setMargin(new Insets(0, 0, 0, 0));
        }
        editorComponent.addFocusListener(spinnerListener);
    }

    @Override
    protected void replaceEditor(final JComponent oldEditor, final JComponent newEditor) {
        super.replaceEditor(oldEditor, newEditor);
        editor = newEditor;
        if (oldEditor != null && oldEditor.getComponents().length > 0) {
            oldEditor.getComponents()[0].removeFocusListener(spinnerListener);
        }
        updateEditorComponent();
    }

    private JButton createArrow(final int direction) {
        Insets insets = UIManager.getInsets("Spinner.arrowButtonInsets");
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

        Rectangle arrowBounds = prevButton.getBounds();

        Color bg = getBackground(c);
        Color spinBg = getArrowBackground(c);
        boolean isCellEditor = SpinnerConstants.isTreeOrTableCellEditor(c);

        if (editorComponent != null) {
            PropertyUtil.installBackground(editorComponent, bg);
            bg = editorComponent.getBackground();
        }

        DividedWidgetPainter.paintBackground((Graphics2D) g, c, arc, arrowBounds, bg, spinBg, isCellEditor);
    }

    protected Color getBackground(final JComponent c) {
        return c == null ? inactiveBackground : c.getBackground();
    }

    protected void updateBackground() {
        PropertyUtil.installBackground(spinner, spinner.isEnabled() ? background : inactiveBackground);
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
