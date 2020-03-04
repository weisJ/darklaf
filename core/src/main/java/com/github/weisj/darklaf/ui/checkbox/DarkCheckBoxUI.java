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
package com.github.weisj.darklaf.ui.checkbox;

import com.github.weisj.darklaf.icons.EmptyIcon;
import com.github.weisj.darklaf.util.DarkSwingUtil;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.GraphicsContext;
import com.github.weisj.darklaf.util.GraphicsUtil;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.IconUIResource;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.plaf.metal.MetalCheckBoxUI;
import javax.swing.text.View;
import java.awt.*;
import java.awt.geom.Path2D;
import java.awt.geom.RoundRectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkCheckBoxUI extends MetalCheckBoxUI implements PropertyChangeListener {

    private static final int ICON_OFF = 4;
    private static final int SIZE = 13;
    private static final Rectangle viewRect = new Rectangle();
    private static final Rectangle iconRect = new Rectangle();
    private static final Rectangle textRect = new Rectangle();
    private static Dimension size = new Dimension();
    private final RoundRectangle2D hitArea = new RoundRectangle2D.Float();
    protected JCheckBox checkBox;
    protected int arcSize;
    protected int borderSize;
    protected Color background;
    protected Color inactiveBackground;
    protected Color selectedBackground;
    protected Color borderColor;
    protected Color focusBorderColor;
    protected Color inactiveBorderColor;
    protected Color selectedBorderColor;
    protected Color checkColor;
    protected Color inactiveCheckColor;
    protected Color focusCheckColor;
    protected Color focusSelectedBorderColor;
    private Icon checkBoxIcon;
    private Icon checkBoxDisabledIcon;
    private Icon checkBoxFocusedIcon;
    private Icon checkBoxSelectedIcon;
    private Icon checkBoxSelectedDisabledIcon;
    private Icon checkBoxSelectedFocusedIcon;


    public static ComponentUI createUI(final JComponent c) {
        return new DarkCheckBoxUI();
    }

    @Override
    public void installUI(final JComponent c) {
        checkBox = (JCheckBox) c;
        super.installUI(c);
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        checkBox = null;
    }

    @Override
    public void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        LookAndFeel.installProperty(b, "opaque", false);
        checkBoxIcon = UIManager.getIcon("CheckBox.unchecked.icon");
        checkBoxDisabledIcon = UIManager.getIcon("CheckBox.uncheckedDisabled.icon");
        checkBoxFocusedIcon = UIManager.getIcon("CheckBox.uncheckedFocused.icon");
        checkBoxSelectedIcon = UIManager.getIcon("CheckBox.selected.icon");
        checkBoxSelectedDisabledIcon = UIManager.getIcon("CheckBox.selectedDisabled.icon");
        checkBoxSelectedFocusedIcon = UIManager.getIcon("CheckBox.selectedFocused.icon");
        arcSize = UIManager.getInt("CheckBox.arc");
        borderSize = UIManager.getInt("CheckBox.borderThickness");
        background = UIManager.getColor("CheckBox.activeFillColor");
        inactiveBackground = UIManager.getColor("CheckBox.inactiveFillColor");
        borderColor = UIManager.getColor("CheckBox.activeBorderColor");
        focusBorderColor = UIManager.getColor("CheckBox.focusBorderColor");
        inactiveBorderColor = UIManager.getColor("CheckBox.inactiveBorderColor");
        checkColor = UIManager.getColor("CheckBox.selectionSelectedColor");
        inactiveCheckColor = UIManager.getColor("CheckBox.selectionDisabledColor");
        selectedBorderColor = UIManager.getColor("CheckBox.selectedBorderColor");
        selectedBackground = UIManager.getColor("CheckBox.selectedFillColor");
        focusCheckColor = UIManager.getColor("CheckBox.selectionFocusSelectedColor");
        focusSelectedBorderColor = UIManager.getColor("CheckBox.focusSelectedBorderColor");
    }

    @Override
    protected void installListeners(final AbstractButton button) {
        super.installListeners(button);
        button.addPropertyChangeListener(this);
    }

    @Override
    protected void uninstallListeners(final AbstractButton button) {
        super.uninstallListeners(button);
        button.removePropertyChangeListener(this);
    }

    public static void paintText(final Graphics2D g, final AbstractButton b,
                                 final Rectangle textRect, final String text, final FontMetrics fm,
                                 final Color disabledTextColor) {
        GraphicsContext context = GraphicsUtil.setupAntialiasing(g);
        g.setFont(b.getFont());
        View view = (View) b.getClientProperty(BasicHTML.propertyKey);
        if (view != null) {
            view.paint(g, textRect);
        } else {
            g.setColor(b.isEnabled() ? b.getForeground() : disabledTextColor);
            DarkSwingUtil.drawStringUnderlineCharAt(b, g, text,
                                                    b.getDisplayedMnemonicIndex(),
                                                    textRect.x,
                                                    textRect.y + fm.getAscent());
        }
        context.restore();
    }

    protected String layoutCheckBox(final JCheckBox b, final FontMetrics fm) {
        Insets i = b.getInsets();
        size = b.getSize(size);
        viewRect.x = i.left;
        viewRect.y = i.top;
        viewRect.width = size.width - (i.right + viewRect.x);
        viewRect.height = size.height - (i.bottom + viewRect.y);
        iconRect.x = iconRect.y = iconRect.width = iconRect.height = 0;
        textRect.x = textRect.y = textRect.width = textRect.height = 0;

        String text = SwingUtilities.layoutCompoundLabel(b, fm, b.getText(), getDefaultIcon(),
                                                         b.getVerticalAlignment(), b.getHorizontalAlignment(),
                                                         b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
                                                         viewRect, iconRect, textRect, b.getIconTextGap());
        if (DarkCheckBoxBorder.isTableCellEditor(b) || DarkCheckBoxBorder.isTreeCellEditor(b)) {
            hitArea.setRoundRect(Math.max(iconRect.x, 0) + ICON_OFF,
                                 Math.max(iconRect.y, 0) + ICON_OFF,
                                 SIZE, SIZE, arcSize, arcSize);
        } else {
            int x = Math.min(iconRect.x, textRect.x);
            int y = Math.min(iconRect.y, textRect.y);
            int xEnd = Math.max(iconRect.x + iconRect.width, textRect.x + textRect.width);
            int yEnd = Math.max(iconRect.y + iconRect.height, textRect.y + textRect.y);
            hitArea.setRoundRect(x, y, xEnd - x, yEnd - y, 0, 0);
        }
        return text;
    }

    private void paintBackground(final JComponent c, final Graphics2D g) {
        if (c.isOpaque()) {
            g.setColor(c.getBackground());
            g.fillRect(0, 0, c.getWidth(), c.getHeight());
        }
    }

    public static Icon getIconBullet(final JComponent c, final Graphics2D g, final AbstractButton b) {
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

    protected Icon getCheckIcon(final AbstractButton b) {
        boolean selected = b.isSelected();
        boolean enabled = b.isEnabled();
        boolean hasFocus = b.hasFocus();
        return selected ? enabled ? hasFocus ? checkBoxSelectedFocusedIcon : checkBoxSelectedIcon
                                  : checkBoxSelectedDisabledIcon
                        : enabled ? hasFocus ? checkBoxFocusedIcon : checkBoxIcon
                                  : checkBoxDisabledIcon;
    }

    protected void paintDarkCheck(final JComponent c, final Graphics2D g, final JCheckBox b) {
        GraphicsContext config = new GraphicsContext(g);
        boolean enabled = b.isEnabled();
        g.translate(iconRect.x + ICON_OFF, iconRect.y + ICON_OFF);

        paintCheckBorder(g, enabled, b.hasFocus() && b.isFocusPainted(), b.isSelected(), arcSize, borderSize);
        if (b.isSelected()) {
            paintCheckArrow(g, enabled, b.hasFocus() && b.isFocusPainted());
        }
        g.translate(-iconRect.x - ICON_OFF, -iconRect.y - ICON_OFF);
        config.restore();
    }

    @Override
    public synchronized void paint(final Graphics g2d, final JComponent c) {
        Graphics2D g = (Graphics2D) g2d;
        JCheckBox b = (JCheckBox) c;
        FontMetrics fm = DarkSwingUtil.getFontMetrics(c, g, c.getFont());

        String text = layoutCheckBox(b, fm);

        paintBackground(c, g);

        Icon icon = getIconBullet(c, g, b);
        if (icon != null) {
            icon.paintIcon(c, g, iconRect.x, iconRect.y);
        } else {
            Icon checkIcon = getCheckIcon(b);
            if (checkIcon != null) {
                checkIcon.paintIcon(c, g, iconRect.x, iconRect.y + 1);
            } else {
                paintDarkCheck(c, g, b);
            }
        }

        if (text != null) {
            paintText(g, b, textRect, text, fm, getDisabledTextColor());
        }
    }

    @Override
    public Icon getDefaultIcon() {
        return new IconUIResource(EmptyIcon.create(20));
    }

    protected void paintCheckBorder(final Graphics2D g, final boolean enabled, final boolean focus,
                                    final boolean selected, final int arcSize, final int borderSize) {
        Graphics2D g2 = (Graphics2D) g.create();
        Color bgColor = getFillColor(selected, enabled);
        Color border = getBorderColor(selected, focus, enabled);
        g.setColor(bgColor);
        DarkUIUtil.fillRoundRect(g, 0, 0, SIZE, SIZE, arcSize);

        if (focus) {
            g2.translate(-borderSize, -borderSize);
            DarkUIUtil.paintFocusBorder(g2, SIZE + 2 * borderSize,
                                        SIZE + 2 * borderSize, arcSize, borderSize);
            g2.translate(borderSize, borderSize);
        }

        g.setColor(border);
        DarkUIUtil.paintLineBorder(g, 0, 0, SIZE, SIZE, arcSize);

        g2.dispose();
    }

    protected void paintCheckArrow(final Graphics2D g, final boolean enabled, final boolean focus) {
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        g.setStroke(new BasicStroke(2.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        Color color = getCheckColor(focus, enabled);

        g.setPaint(color);
        Path2D check = new Path2D.Float(Path2D.WIND_EVEN_ODD);
        check.moveTo(2.5, 8);
        check.lineTo(5.5, SIZE - 3);
        check.lineTo(SIZE - 2.7, 3);
        g.draw(check);
        config.restore();
    }

    protected Color getFillColor(final boolean selected, final boolean enabled) {
        return enabled ? selected ? selectedBackground : background
                       : inactiveBorderColor;
    }

    protected Color getBorderColor(final boolean selected, final boolean focus, final boolean enabled) {
        return enabled ? focus ? selected ? focusSelectedBorderColor : focusBorderColor
                               : selected ? selectedBorderColor : borderColor
                       : inactiveBorderColor;
    }

    protected Color getCheckColor(final boolean focus, final boolean enabled) {
        return enabled ? focus ? focusCheckColor : checkColor
                       : inactiveCheckColor;
    }

    @Override
    public boolean contains(final JComponent c, final int x, final int y) {
        if (hitArea.isEmpty() && c instanceof JCheckBox) {
            layoutCheckBox((JCheckBox) c, c.getFontMetrics(c.getFont()));
        }
        return hitArea.contains(x, y);
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if ("componentOrientation".equals(key)) {
            checkBox.repaint();
            hitArea.setRoundRect(0, 0, 0, 0, 0, 0);
        } else if ("JToggleButton.isTreeCellEditor".equals(key) || "JToggleButton.isTableCellEditor".equals(key)) {
            checkBox.repaint();
            hitArea.setRoundRect(0, 0, 0, 0, 0, 0);
        } else if ("JToggleButton.clearHitArea".equals(key)) {
            hitArea.setRoundRect(0, 0, 0, 0, 0, 0);
        }
    }
}
