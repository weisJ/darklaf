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
package com.github.weisj.darklaf.ui.button;

import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.GraphicsContext;
import com.github.weisj.darklaf.util.GraphicsUtil;
import com.github.weisj.darklaf.util.SystemInfo;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicButtonUI;
import javax.swing.plaf.basic.BasicGraphicsUtils;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.View;
import java.awt.*;
import java.awt.geom.RoundRectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkButtonUI extends BasicButtonUI implements PropertyChangeListener {

    protected static final Rectangle viewRect = new Rectangle();
    protected static final Rectangle textRect = new Rectangle();
    protected static final Rectangle iconRect = new Rectangle();
    protected int borderSize;
    protected int shadowHeight;
    protected Color inactiveForeground;
    protected Color defaultForeground;
    protected Color defaultBackground;
    protected Color defaultHoverBackground;
    protected Color defaultClickBackground;
    protected Color background;
    protected Color hoverBackground;
    protected Color clickBackground;
    protected Color inactiveBackground;
    protected Color shadowHover;
    protected Color shadowClick;
    protected AbstractButton button;
    private int arc;
    private int squareArc;

    protected final AbstractButtonLayoutDelegate layoutDelegate = new AbstractButtonLayoutDelegate() {
        @Override
        public Font getFont() {
            return delegate != null ? delegate.getFont().deriveFont(Font.BOLD) : null;
        }
    };
    protected boolean oldRolloverEnabled;
    protected boolean oldThin;
    protected boolean oldSquare;
    protected boolean oldAltArc;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkButtonUI();
    }

    public static boolean chooseAlternativeArc(final Component c) {
        return c instanceof JButton
            && Boolean.TRUE.equals(((JButton) c).getClientProperty("JButton.alternativeArc"));
    }

    @Override
    protected void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        LookAndFeel.installProperty(b, "opaque", false);
        b.setRolloverEnabled(UIManager.getBoolean("Button.rolloverEnabled"));
        borderSize = UIManager.getInt("Button.borderThickness");
        shadowHeight = UIManager.getInt("Button.shadowHeight");
        inactiveForeground = UIManager.getColor("Button.disabledText");
        defaultForeground = UIManager.getColor("Button.selectedButtonForeground");
        defaultBackground = UIManager.getColor("Button.defaultFillColor");
        defaultHoverBackground = UIManager.getColor("Button.defaultFillColorRollOver");
        defaultClickBackground = UIManager.getColor("Button.defaultFillColorClick");
        background = UIManager.getColor("Button.activeFillColor");
        hoverBackground = UIManager.getColor("Button.activeFillColorRollOver");
        clickBackground = UIManager.getColor("Button.activeFillColorClick");
        inactiveBackground = UIManager.getColor("Button.inactiveFillColor");
        shadowHover = UIManager.getColor("Button.shadow.hover");
        shadowClick = UIManager.getColor("Button.shadow.click");
        arc = UIManager.getInt("Button.arc");
        squareArc = UIManager.getInt("Button.squareArc");
    }

    @Override
    protected void installListeners(final AbstractButton b) {
        super.installListeners(b);
        b.addPropertyChangeListener(this);
    }

    @Override
    protected void uninstallListeners(final AbstractButton b) {
        super.uninstallListeners(b);
        b.removePropertyChangeListener(this);
    }

    public static boolean isLabelButton(final Component c) {
        return c instanceof JButton
            && "onlyLabel".equals(((JButton) c).getClientProperty("JButton.variant"));
    }

    @Override
    protected void paintText(final Graphics g, final JComponent c,
                             final Rectangle textRect, final String text) {
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        AbstractButton button = (AbstractButton) c;
        ButtonModel model = button.getModel();
        g.setColor(getForeground(button));
        FontMetrics metrics = SwingUtilities2.getFontMetrics(c, g);
        int mnemonicIndex = button.getDisplayedMnemonicIndex();
        if (!model.isEnabled()) {
            g.setColor(inactiveForeground);
            mnemonicIndex = -1;
        }
        SwingUtilities2.drawStringUnderlineCharAt(c, g, text, mnemonicIndex,
                                                  textRect.x + getTextShiftOffset(),
                                                  textRect.y + metrics.getAscent() + getTextShiftOffset());
        config.restore();
    }

    public static boolean isShadowVariant(final Component c) {
        if (isFullShadow(c)) return true;
        if (c instanceof JButton) {
            JButton b = (JButton) c;
            return (isIconOnly(b) && convertIconButtonToShadow(b))
                || "shadow".equals(b.getClientProperty("JButton.variant"));
        }
        return false;
    }

    protected static boolean isIconOnly(final AbstractButton b) {
        return b.getIcon() != null && (b.getText() == null || b.getText().isEmpty());
    }

    protected boolean isDefaultButton(final JComponent c) {
        return c instanceof JButton && ((JButton) c).isDefaultButton();
    }

    protected Color getForeground(final AbstractButton button) {
        Color fg = button.getForeground();
        if (fg instanceof UIResource && isDefaultButton(button) && !isShadowVariant(button)) {
            fg = defaultForeground;
        }
        return fg;
    }

    public static boolean isFullShadow(final Component c) {
        return c instanceof JButton
            && "fullShadow".equals(((JButton) c).getClientProperty("JButton.variant"));
    }

    private boolean shouldDrawBackground(final JComponent c) {
        if (isLabelButton(c)) return false;
        AbstractButton button = (AbstractButton) c;
        Border border = c.getBorder();
        return c.isEnabled() && border != null && button.isContentAreaFilled();
    }

    protected Color getShadowColor(final AbstractButton c) {
        Object colorHover = c.getClientProperty("JButton.shadow.hover");
        Object colorClick = c.getClientProperty("JButton.shadow.click");
        return c.getModel().isArmed() ? colorClick instanceof Color ? (Color) colorClick : shadowClick
                                      : colorHover instanceof Color ? (Color) colorHover : shadowHover;
    }

    protected Color getBackgroundColor(final JComponent c) {
        boolean defaultButton = isDefaultButton(c);
        boolean rollOver = c instanceof JButton && (((JButton) c).isRolloverEnabled()
            && (((JButton) c).getModel().isRollover()));
        boolean clicked = c instanceof JButton && (((JButton) c).getModel().isArmed());
        if (c.isEnabled()) {
            if (defaultButton) {
                if (clicked) {
                    return defaultClickBackground;
                } else if (rollOver) {
                    return defaultHoverBackground;
                } else {
                    return defaultBackground;
                }
            } else {
                if (clicked) {
                    return clickBackground;
                } else if (rollOver) {
                    return hoverBackground;
                } else {
                    return background;
                }
            }
        } else {
            return inactiveBackground;
        }
    }

    protected int getArc(final Component c) {
        if (DarkButtonUI.isNoArc(c)) return 0;
        boolean square = DarkButtonUI.isSquare(c);
        boolean alt = DarkButtonUI.chooseAlternativeArc(c);
        return square ? alt ? arc : squareArc : alt ? squareArc : arc;
    }

    protected String layout(final AbstractButton b, final JComponent c, final FontMetrics fm,
                            final int width, final int height) {
        Insets i = b.getInsets();
        viewRect.x = i.left;
        viewRect.y = i.top;
        viewRect.width = width - (i.right + viewRect.x);
        viewRect.height = height - (i.bottom + viewRect.y);

        textRect.x = textRect.y = textRect.width = textRect.height = 0;
        iconRect.x = iconRect.y = iconRect.width = iconRect.height = 0;

        // layout the text and icon
        return SwingUtilities.layoutCompoundLabel(
                b, fm, b.getText(), b.getIcon(),
                b.getVerticalAlignment(), b.getHorizontalAlignment(),
                b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
                viewRect, iconRect, textRect,
                b.getText() == null ? 0 : b.getIconTextGap());
    }

    protected void paintText(final Graphics g, final AbstractButton b, final JComponent c, final String text) {
        GraphicsContext context = GraphicsUtil.setupAntialiasing(g);
        g.setClip(textRect);
        if (text != null && !text.equals("")) {
            View v = (View) c.getClientProperty(BasicHTML.propertyKey);
            if (v != null) {
                v.paint(g, textRect);
            } else {
                paintText(g, b, textRect, text);
            }
        }
        context.restore();
    }

    protected void paintIcon(final Graphics g, final AbstractButton b, final JComponent c) {
        if (b.getIcon() != null) {
            g.setClip(iconRect);
            paintIcon(g, c, iconRect);
        }
    }

    public static boolean isNoArc(final Component c) {
        return c instanceof JButton
            && Boolean.TRUE.equals(((JButton) c).getClientProperty("JButton.noArc"));
    }

    public static boolean isSquare(final Component c) {
        return c instanceof JButton && Boolean.TRUE.equals(((JButton) c).getClientProperty("JButton.square"));
    }

    public static boolean isThin(final Component c) {
        return c instanceof JButton
            && Boolean.TRUE.equals(((JButton) c).getClientProperty("JButton.thin"));
    }

    protected static boolean convertIconButtonToShadow(final AbstractButton b) {
        return !(b instanceof UIResource)
            && UIManager.getBoolean("Button.convertIconOnlyToShadow")
            && !Boolean.TRUE.equals(b.getClientProperty("JButton.noShadowOverwrite"));
    }

    @Override
    public void installUI(final JComponent c) {
        button = (AbstractButton) c;
        super.installUI(c);
        oldRolloverEnabled = button.isRolloverEnabled();
        updateRolloverEnabled();
    }

    public void updateRolloverEnabled() {
        if (isIconOnly(button) && convertIconButtonToShadow(button)) {
            oldRolloverEnabled = button.isRolloverEnabled();
            oldThin = isThin(button);
            oldSquare = isSquare(button);
            oldAltArc = chooseAlternativeArc(button);
            button.setRolloverEnabled(true);
            button.putClientProperty("JButton.square", true);
            button.putClientProperty("JButton.thin", true);
            button.putClientProperty("JButton.alternativeArc", true);
        } else {
            button.setRolloverEnabled(oldRolloverEnabled);
            button.putClientProperty("JButton.square", oldSquare);
            button.putClientProperty("JButton.thin", oldThin);
            button.putClientProperty("JButton.alternativeArc", oldAltArc);
        }
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        AbstractButton b = (AbstractButton) c;
        layoutDelegate.setDelegate(b);
        return BasicGraphicsUtils.getPreferredButtonSize(layoutDelegate, b.getIconTextGap());
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        GraphicsContext config = new GraphicsContext(g);
        AbstractButton b = (AbstractButton) c;
        paintButton(g, c);

        if (isDefaultButton(b)) {
            g.setFont(g.getFont().deriveFont(Font.BOLD));
        } else if (g.getFont().isBold()) {
            g.setFont(g.getFont().deriveFont(Font.PLAIN));
        }

        String text = layout(b, c, SwingUtilities2.getFontMetrics(b, g),
                             b.getWidth() + 2, b.getHeight());

        paintIcon(g, b, c);
        paintText(g, b, c, text);
        config.restore();
    }

    protected void paintButton(final Graphics g, final JComponent c) {
        Graphics2D g2 = (Graphics2D) g;
        if (shouldDrawBackground(c)) {
            AbstractButton b = (AbstractButton) c;
            int arc = getArc(c);
            Insets margin = b.getMargin();
            if (margin instanceof UIResource) margin = new Insets(0, 0, 0, 0);
            if (isShadowVariant(c)) {
                if (b.isEnabled() && b.getModel().isRollover()) {
                    GraphicsUtil.setupAAPainting(g2);
                    g.setColor(getShadowColor(b));
                    if (isFullShadow(c)) {
                        g.fillRect(margin.left, margin.top,
                                   c.getWidth() - margin.left - margin.right,
                                   c.getHeight() - margin.top - margin.bottom);
                    } else {
                        g.fillRoundRect(margin.left, margin.top,
                                        c.getWidth() - margin.left - margin.right,
                                        c.getHeight() - margin.top - margin.bottom,
                                        arc, arc);
                    }
                }
            } else {
                g2.setColor(getBackgroundColor(c));
                if (isSquare(c) && !chooseAlternativeArc(c)) {
                    g2.fillRect(borderSize, borderSize, c.getWidth() - 2 * borderSize,
                                c.getHeight() - 2 * borderSize - shadowHeight);
                } else {
                    DarkUIUtil.fillRoundRect((Graphics2D) g, borderSize, borderSize, c.getWidth() - 2 * borderSize,
                                             c.getHeight() - 2 * borderSize - shadowHeight, arc);
                }
            }
        }
    }

    @Override
    public void update(final Graphics g, final JComponent c) {
        super.update(g, c);
        boolean isDefaultButton = isDefaultButton(c) && !SystemInfo.isMac;
        if (isDefaultButton && !c.getFont().isBold()) {
            c.setFont(c.getFont().deriveFont(Font.BOLD));
        } else if (!isDefaultButton && c.getFont().isBold()) {
            c.setFont(c.getFont().deriveFont(Font.PLAIN));
        }
    }

    @Override
    public boolean contains(final JComponent c, final int x, final int y) {
        if (isLabelButton(c)) {
            return super.contains(c, x, y);
        }
        if (!(x >= 0 && x <= c.getWidth() && y >= 0 && y <= c.getHeight())) return false;
        int bs = borderSize;
        int arc = getArc(c);
        return new RoundRectangle2D.Float(bs, bs, c.getWidth() - 2 * bs, c.getWidth() - 2 * bs,
                                          arc, arc).contains(x, y);
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if (key.startsWith("JButton.")) {
            button.repaint();
            button.revalidate();
        } else if (JButton.TEXT_CHANGED_PROPERTY.equals(key)) {
            updateRolloverEnabled();
        }
    }
}
