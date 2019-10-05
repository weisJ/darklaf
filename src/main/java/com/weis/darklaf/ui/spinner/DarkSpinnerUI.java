package com.weis.darklaf.ui.spinner;

import com.weis.darklaf.components.ArrowButton;
import com.weis.darklaf.decorators.LayoutManagerDelegate;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicSpinnerUI;
import java.awt.*;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.Area;
import java.awt.geom.RoundRectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class DarkSpinnerUI extends BasicSpinnerUI implements PropertyChangeListener {

    private static final int BUTTON_PAD = 7;

    private Component component;
    private JComponent editor;
    private Color compColor;
    private JButton prevButton;

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

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkSpinnerUI();
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

    protected static boolean isTableCellEditor(@NotNull final Component c) {
        return c instanceof JComponent
               && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JSpinner.isTableCellEditor"));
    }

    protected static boolean isTreeCellEditor(@NotNull final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JSpinner.isTreeCellEditor"));
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
            var comp = newEditor.getComponents()[0];
            comp.addFocusListener(focusListener);
        }
    }

    @Override
    protected JComponent createEditor() {
        editor = super.createEditor();
        component = ((JSpinner.DefaultEditor) editor).getTextField();
        component.addFocusListener(focusListener);
        return editor;
    }

    private void paintSpinBackground(@NotNull final Graphics2D g, final int width, final int height,
                                     final int bSize, final int arc) {
        var bounds = prevButton.getBounds();
        boolean leftToRight = spinner.getComponentOrientation().isLeftToRight();
        int off = leftToRight ? bounds.x + 1 : bounds.x + bounds.width;
        Area rect = new Area(new RoundRectangle2D.Double(bSize, bSize, width - 2 * bSize, height - 2 * bSize,
                                                         arc, arc));
        Area iconRect = new Area(new Rectangle(off, bSize, width - 2 * bSize - off + 1, height - 2 * bSize));
        if (leftToRight) {
            rect.intersect(iconRect);
        } else {
            rect.subtract(iconRect);
        }
        g.setColor(getSpinBackground());
        g.fill(rect);
    }

    @Override
    protected Component createPreviousButton() {
        prevButton = createArrow(SwingConstants.SOUTH);
        prevButton.setName("Spinner.previousButton");
        prevButton.setBorder(new EmptyBorder(1, 1, 1, 1));
        installPreviousButtonListeners(prevButton);
        return prevButton;
    }

    @Override
    protected Component createNextButton() {
        JButton nextButton = createArrow(SwingConstants.NORTH);
        nextButton.setName("Spinner.nextButton");
        nextButton.setBorder(new EmptyBorder(1, 1, 1, 1));
        installNextButtonListeners(nextButton);
        return nextButton;
    }

    @NotNull
    private JButton createArrow(final int direction) {
        var insets = new Insets(0, BUTTON_PAD, 0, BUTTON_PAD);
        JButton button = ArrowButton.createUpDownArrow(spinner, direction, false, true, insets);
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

    @Override
    public void paint(final Graphics g, @NotNull final JComponent c) {
        int size = DarkSpinnerBorder.BORDER_SIZE;
        int arc = DarkSpinnerBorder.ARC_SIZE;
        int width = c.getWidth();
        int height = c.getHeight();
        JComponent editor = spinner.getEditor();
        if (c.isOpaque()) {
            if (component != null) {
                if (!component.isEnabled()) {
                    if (compColor == null) {
                        compColor = component.getBackground();
                        component.setBackground(getBackground(c));
                    } else {
                        compColor = null;
                    }
                }
                g.setColor(component.getBackground());
            } else {
                g.setColor(getBackground(c));
            }
            if (!isTableCellEditor(c) && !isTreeCellEditor(c)) {
                g.fillRoundRect(size, size, width - 2 * size, height - 2 * size, arc, arc);
            } else {
                var bounds = prevButton.getBounds();
                boolean leftToRight = spinner.getComponentOrientation().isLeftToRight();
                int off = leftToRight ? bounds.x + 1 : bounds.x + bounds.width;
                if (leftToRight) {
                    g.fillRect(0, 0, off, height);
                } else {
                    g.fillRect(off, 0, width - off, height);
                }
            }
        }
        if (editor != null) {
            paintSpinBackground((Graphics2D) g, width, height, size, arc);
        }
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
                    var bounds = editor.getBounds();
                    bounds.x += DarkSpinnerBorder.BORDER_SIZE;
                    bounds.width -= DarkSpinnerBorder.BORDER_SIZE;
                    editor.setBounds(bounds);
                }
            }
        };
    }

    public static Color getSpinBackground() {
        return getBackground(null);
    }

    public static Color getBackground(final JComponent c) {
        return c == null || !c.isEnabled() ? UIManager.getColor("Spinner.inactiveBackground")
                                           : UIManager.getColor("Spinner.activeBackground");
    }

    @Override
    public void propertyChange(@NotNull final PropertyChangeEvent evt) {
        var key = evt.getPropertyName();
        if ("opaque".equals(key)) {
            var val = Boolean.TRUE.equals(evt.getNewValue());
            spinner.getEditor().setOpaque(val);
            if (component instanceof JComponent) {
                ((JComponent) component).setOpaque(val);
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
        } else if ("JSpinner.cellEditorAlignment".equals(key) && isTableCellEditor(spinner)) {
            if (component instanceof JTextField && evt.getNewValue() instanceof Integer) {
                ((JTextField) component).setHorizontalAlignment((Integer) evt.getNewValue());
            }
        }
    }
}
