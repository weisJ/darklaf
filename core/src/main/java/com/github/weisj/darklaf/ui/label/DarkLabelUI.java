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
package com.github.weisj.darklaf.ui.label;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicLabelUI;

import sun.swing.SwingUtilities2;

import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.StringPainter;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyKey;

/** @author Jannis Weis */
public class DarkLabelUI extends BasicLabelUI implements PropertyChangeListener {

    protected static DarkLabelUI darkLabelUI;

    private Color inactiveForeground;

    protected final Rectangle paintIconR = new Rectangle();
    protected final Rectangle paintTextR = new Rectangle();

    public DarkLabelUI() {
        installUI(null);
    }

    public static ComponentUI createUI(final JComponent c) {
        if (darkLabelUI == null) darkLabelUI = new DarkLabelUI();
        return darkLabelUI;
    }

    @Override
    public void installUI(final JComponent c) {
        if (c != null) super.installUI(c);
    }

    @Override
    protected void installDefaults(final JLabel c) {
        super.installDefaults(c);
        LookAndFeel.installProperty(c, PropertyKey.OPAQUE, false);
        inactiveForeground = UIManager.getColor("Label.inactiveForeground");
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        GraphicsContext config = new GraphicsContext(g);
        JLabel label = (JLabel) c;
        String text = label.getText();
        Icon icon = getIcon(label);

        paintBackground(g, c);

        if ((icon == null) && (text == null)) {
            return;
        }

        FontMetrics fm = SwingUtilities2.getFontMetrics(label, g);
        String clippedText = layout(label, fm, c.getWidth(), c.getHeight());

        if (icon != null) {
            icon.paintIcon(c, g, paintIconR.x, paintIconR.y);
            config.restoreClip();
        }

        paintText(g, label, fm, clippedText);
    }

    protected void paintBackground(final Graphics g, final JComponent c) {}

    public void paintText(final Graphics g, final JLabel label, final FontMetrics fm, final String clippedText) {
        int mnemIndex = label.isEnabled() ? label.getDisplayedMnemonicIndex() : -1;
        g.setColor(getForeground(label));
        StringPainter.drawStringUnderlineCharAt(g, label, clippedText, mnemIndex, paintTextR, label.getFont(), fm);
    }

    protected Color getForeground(final Component label) {
        if (label.isEnabled()) {
            return getEnabledForeground(label);
        } else {
            return getDisabledForeground(label);
        }
    }

    protected Color getEnabledForeground(final Component label) {
        return label.getForeground();
    }

    protected Color getDisabledForeground(final Component label) {
        if (!DarkUIUtil.isInCell(label)) {
            return inactiveForeground;
        }
        return getEnabledForeground(label);
    }

    protected Icon getIcon(final JLabel label) {
        Icon icon;
        if (label.isEnabled()) {
            icon = label.getIcon();
        } else {
            icon = label.getDisabledIcon();
        }
        return icon;
    }

    protected String layout(final JLabel label, final FontMetrics fm, final int width, final int height) {
        Insets insets = label.getInsets(null);
        String text = label.getText();
        Icon icon = getIcon(label);
        Rectangle paintViewR = new Rectangle();
        paintViewR.x = insets.left;
        paintViewR.y = insets.top;
        paintViewR.width = width - (insets.left + insets.right);
        paintViewR.height = height - (insets.top + insets.bottom);
        paintIconR.x = paintIconR.y = paintIconR.width = paintIconR.height = 0;
        paintTextR.x = paintTextR.y = paintTextR.width = paintTextR.height = 0;
        return layoutCL(label, fm, text, icon, paintViewR, paintIconR, paintTextR);
    }

    @Override
    public void propertyChange(final PropertyChangeEvent e) {
        super.propertyChange(e);
        String key = e.getPropertyName();
        if (PropertyKey.COMPONENT_ORIENTATION.equals(key)) {
            Object source = e.getSource();
            if (source instanceof JLabel) {
                ((JLabel) source).doLayout();
                ((JLabel) source).repaint();
            }
        }
    }
}
