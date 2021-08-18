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
package com.github.weisj.darklaf.ui.togglebutton.radiobutton;

import java.awt.*;
import java.awt.geom.Ellipse2D;
import java.awt.geom.RectangularShape;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicButtonListener;
import javax.swing.plaf.metal.MetalRadioButtonUI;

import com.github.weisj.darklaf.compatibility.SwingUtil;
import com.github.weisj.darklaf.graphics.StringPainter;
import com.github.weisj.darklaf.swingdsl.VisualPaddingListener;
import com.github.weisj.darklaf.ui.togglebutton.DarkToggleButtonUI;
import com.github.weisj.darklaf.ui.togglebutton.ToggleButtonConstants;
import com.github.weisj.darklaf.ui.togglebutton.ToggleButtonFocusNavigationActions;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;

/** @author Jannis Weis */
public class DarkRadioButtonUI extends MetalRadioButtonUI implements PropertyChangeListener, ToggleButtonConstants {

    protected final Rectangle viewRect = new Rectangle();
    protected final Rectangle iconRect = new Rectangle();
    protected final Rectangle textRect = new Rectangle();
    protected Dimension size = new Dimension();
    protected RectangularShape hitArea;
    protected JToggleButton radioButton;

    protected String displayString;

    private Icon stateIcon;
    protected BasicButtonListener buttonListener;
    protected ToggleButtonFocusNavigationActions keyboardAction;
    protected VisualPaddingListener visualPaddingListener;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkRadioButtonUI();
    }

    @Override
    public void installUI(final JComponent c) {
        radioButton = (JToggleButton) c;
        super.installUI(c);
    }

    @Override
    public void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        LookAndFeel.installProperty(b, PropertyKey.OPAQUE, false);
        hitArea = new Rectangle();
        installIcons();
        radioButton.setLayout(createLayout());
    }

    protected LayoutManager createLayout() {
        return new DarkRadioButtonLayout();
    }

    protected void installIcons() {
        stateIcon = UIManager.getIcon("RadioButton.icon");
    }

    @Override
    protected void installListeners(final AbstractButton button) {
        buttonListener = createButtonListener(button);
        button.addMouseListener(buttonListener);
        button.addMouseMotionListener(buttonListener);
        button.addFocusListener(buttonListener);
        button.addPropertyChangeListener(buttonListener);
        button.addChangeListener(buttonListener);
        button.addPropertyChangeListener(this);
        keyboardAction = new ToggleButtonFocusNavigationActions(button);
        keyboardAction.installActions();
        visualPaddingListener = new VisualPaddingListener();
        button.addPropertyChangeListener(visualPaddingListener);
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        radioButton = null;
    }

    @Override
    protected void uninstallDefaults(final AbstractButton b) {
        super.uninstallDefaults(b);
        radioButton.setLayout(null);
    }

    @Override
    protected void uninstallListeners(final AbstractButton button) {
        button.removeMouseListener(buttonListener);
        button.removeMouseMotionListener(buttonListener);
        button.removeFocusListener(buttonListener);
        button.removeChangeListener(buttonListener);
        button.removePropertyChangeListener(buttonListener);
        button.removePropertyChangeListener(this);
        keyboardAction.uninstallActions();
        keyboardAction = null;
        button.removePropertyChangeListener(visualPaddingListener);
        visualPaddingListener = null;
    }

    @Override
    // We're not calling the super implementation
    @SuppressWarnings("UnsynchronizedOverridesSynchronized")
    public void paint(final Graphics g2d, final JComponent c) {
        Graphics2D g = (Graphics2D) g2d;
        AbstractButton b = (AbstractButton) c;

        g.setFont(c.getFont());
        FontMetrics fm = SwingUtil.getFontMetrics(c, g);

        paintBackground(c, g);
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        Icon icon = getIconBullet(b);
        if (icon != null) {
            icon.paintIcon(c, g, iconRect.x, iconRect.y);
        } else {
            Icon radioIcon = getStateIcon(b);
            if (radioIcon != null) {
                radioIcon.paintIcon(c, g, iconRect.x, iconRect.y);
            }
        }
        config.restore();

        if (displayString != null) {
            paintText(g, b, textRect, displayString, fm, getDisabledTextColor());
        }
    }

    protected void paintBackground(final JComponent c, final Graphics2D g) {
        if (c.isOpaque()) {
            g.setColor(c.getBackground());
            g.fillRect(0, 0, c.getWidth(), c.getHeight());
        }
    }

    public static void paintText(final Graphics2D g, final AbstractButton b, final Rectangle textRect,
            final String text, final FontMetrics fm, final Color disabledTextColor) {
        g.setColor(b.isEnabled() ? b.getForeground() : disabledTextColor);
        int mnemIndex = b.isEnabled() ? b.getDisplayedMnemonicIndex() : -1;
        StringPainter.drawStringUnderlineCharAt(g, b, text, mnemIndex, textRect, b.getFont(), fm);
    }

    protected Icon getStateIcon(final AbstractButton b) {
        return stateIcon;
    }

    public static Icon getIconBullet(final AbstractButton b) {
        ButtonModel model = b.getModel();
        Icon icon = b.getIcon();
        if (!model.isEnabled()) {
            if (model.isSelected()) {
                icon = b.getDisabledSelectedIcon();
            } else {
                icon = b.getDisabledIcon();
            }
        } else if (model.isPressed() && model.isArmed()) {
            icon = b.getPressedIcon();
            if (icon == null) {
                // Use selected icon
                icon = b.getSelectedIcon();
            }
        } else if (model.isSelected()) {
            if (b.isRolloverEnabled() && model.isRollover()) {
                icon = b.getRolloverSelectedIcon();
                if (icon == null) {
                    icon = b.getSelectedIcon();
                }
            } else {
                icon = b.getSelectedIcon();
            }
        } else if (b.isRolloverEnabled() && model.isRollover()) {
            icon = b.getRolloverIcon();
        }

        if (icon == null) {
            icon = b.getIcon();
        }
        return icon;
    }

    @Override
    public Icon getDefaultIcon() {
        return getStateIcon(radioButton);
    }

    protected RectangularShape calculateHitArea() {
        return new Ellipse2D.Float(Math.max(iconRect.x, 0), Math.max(iconRect.y, 0), iconRect.width, iconRect.height);
    }

    @Override
    public boolean contains(final JComponent c, final int x, final int y) {
        if ((hitArea == null || hitArea.isEmpty()) && c instanceof JToggleButton) {
            c.doLayout();
        }
        return hitArea != null && hitArea.contains(x, y);
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if (PropertyKey.COMPONENT_ORIENTATION.equals(key)) {
            radioButton.repaint();
            hitArea.setFrame(0, 0, 0, 0);
        } else if (DarkToggleButtonUI.KEY_IS_TREE_EDITOR.equals(key)
                || DarkToggleButtonUI.KEY_IS_TABLE_EDITOR.equals(key)) {
            radioButton.repaint();
            hitArea.setFrame(0, 0, 0, 0);
        } else if (DarkToggleButtonUI.KEY_CLEAR_HIT_AREA.equals(key)) {
            hitArea.setFrame(0, 0, 0, 0);
        } else if (PropertyKey.BORDER.equals(key)) {
            hitArea.setFrame(0, 0, 0, 0);
        }
    }

    protected class DarkRadioButtonLayout implements LayoutManager {

        @Override
        public void addLayoutComponent(final String name, final Component comp) {}

        @Override
        public void removeLayoutComponent(final Component comp) {}

        @Override
        public Dimension preferredLayoutSize(final Container parent) {
            return null;
        }

        @Override
        public Dimension minimumLayoutSize(final Container parent) {
            return null;
        }

        @Override
        public void layoutContainer(final Container parent) {
            displayString = layout(radioButton, radioButton.getFontMetrics(radioButton.getFont()));
        }

        protected String layout(final AbstractButton b, final FontMetrics fm) {
            Insets i = b.getInsets();
            size = b.getSize(size);
            viewRect.x = i.left;
            viewRect.y = i.top;
            viewRect.width = size.width - (i.right + viewRect.x);
            viewRect.height = size.height - (i.bottom + viewRect.y);
            iconRect.x = iconRect.y = iconRect.width = iconRect.height = 0;
            textRect.x = textRect.y = textRect.width = textRect.height = 0;

            Icon icon = getIconBullet(b);
            if (icon == null) icon = getDefaultIcon();
            String text = SwingUtilities.layoutCompoundLabel(b, fm, b.getText(), icon, b.getVerticalAlignment(),
                    b.getHorizontalAlignment(), b.getVerticalTextPosition(), b.getHorizontalTextPosition(), viewRect,
                    iconRect, textRect, b.getIconTextGap());
            if (ToggleButtonConstants.isInCell(b)) {
                hitArea = calculateHitArea();
            } else {
                int x = Math.min(iconRect.x, textRect.x);
                int y = Math.min(iconRect.y, textRect.y);
                int xEnd = Math.max(iconRect.x + iconRect.width, textRect.x + textRect.width);
                int yEnd = Math.max(iconRect.y + iconRect.height, textRect.y + textRect.y);
                hitArea = new Rectangle(x, y, xEnd - x, yEnd - y);
            }
            return text;
        }
    }
}
