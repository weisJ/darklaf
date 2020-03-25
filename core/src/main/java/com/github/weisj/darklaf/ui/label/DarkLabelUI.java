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
package com.github.weisj.darklaf.ui.label;

import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.GraphicsContext;
import com.github.weisj.darklaf.util.GraphicsUtil;
import com.github.weisj.darklaf.util.PropertyKey;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.plaf.basic.BasicLabelUI;
import javax.swing.text.View;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * @author Jannis Weis
 */
public class DarkLabelUI extends BasicLabelUI implements PropertyChangeListener {

    protected static final DarkLabelUI darkLabelUI = new DarkLabelUI();

    private Color inactiveForeground;
    private Color cellForegroundNoFocus;
    protected final Rectangle paintIconR = new Rectangle();
    protected final Rectangle paintTextR = new Rectangle();

    public DarkLabelUI() {
        installUI(null);
    }

    public static ComponentUI createUI(final JComponent c) {
        return darkLabelUI;
    }

    @Override
    public void installUI(final JComponent c) {
        if (c != null) super.installUI(c);
        //Ensure colors are up to date.
        inactiveForeground = UIManager.getColor("Label.inactiveForeground");
        cellForegroundNoFocus = UIManager.getColor("Label.cellForegroundNoFocus");
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        JLabel label = (JLabel) c;
        String text = label.getText();
        Icon icon = getIcon(label);

        if ((icon == null) && (text == null)) {
            return;
        }

        FontMetrics fm = SwingUtilities2.getFontMetrics(label, g);
        String clippedText = layout(label, fm, c.getWidth(), c.getHeight());

        if (icon != null) {
            icon.paintIcon(c, g, paintIconR.x, paintIconR.y);
        }

        if (text != null) {
            View v = (View) c.getClientProperty(BasicHTML.propertyKey);
            if (v != null) {
                v.paint(g, paintTextR);
            } else {
                int textX = paintTextR.x;
                int textY = paintTextR.y + fm.getAscent();

                if (label.isEnabled()) {
                    paintEnabledText(label, g, clippedText, textX, textY);
                } else {
                    paintDisabledText(label, g, clippedText, textX, textY);
                }
            }
        }
        config.restore();
    }

    @Override
    protected void paintEnabledText(final JLabel l, final Graphics g, final String s,
                                    final int textX, final int textY) {
        int mnemIndex = l.getDisplayedMnemonicIndex();
        boolean focus = DarkUIUtil.hasFocus(l)
                        || DarkUIUtil.hasFocus(DarkUIUtil.getParentOfType(JTree.class, l))
                        || DarkUIUtil.hasFocus(DarkUIUtil.getParentOfType(JTable.class, l))
                        || DarkUIUtil.hasFocus(DarkUIUtil.getParentOfType(JList.class, l))
                        || DarkUIUtil.getParentOfType(JPopupMenu.class, l) != null;
        if (DarkUIUtil.isInCell(l) && !focus) {
            g.setColor(cellForegroundNoFocus);
        } else {
            g.setColor(l.getForeground());
        }
        SwingUtilities2.drawStringUnderlineCharAt(l, g, s, mnemIndex,
                                                  textX, textY);
    }

    @Override
    protected void paintDisabledText(final JLabel l, final Graphics g, final String s,
                                     final int textX, final int textY) {
        int accChar = l.getDisplayedMnemonicIndex();
        g.setColor(inactiveForeground);
        SwingUtilities2.drawStringUnderlineCharAt(l, g, s, accChar,
                                                  textX, textY);
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

    protected String layout(final JLabel label, final FontMetrics fm,
                            final int width, final int height) {
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
        return layoutCL(label, fm, text, icon, paintViewR, paintIconR,
                        paintTextR);
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
