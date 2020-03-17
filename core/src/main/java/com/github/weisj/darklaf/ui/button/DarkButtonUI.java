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

import com.github.weisj.darklaf.util.*;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicButtonListener;
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

    protected static final String KEY_PREFIX = "JButton.";
    public static final String KEY_VARIANT = KEY_PREFIX + "variant";
    public static final String KEY_HOVER_COLOR = KEY_PREFIX + "shadow.hover";
    public static final String KEY_CLICK_COLOR = KEY_PREFIX + "shadow.click";
    public static final String KEY_ALT_ARC = KEY_PREFIX + "alternativeArc";
    public static final String KEY_NO_ARC = KEY_PREFIX + "noArc";
    public static final String KEY_SQUARE = KEY_PREFIX + "square";
    public static final String KEY_THIN = KEY_PREFIX + "thin";
    public static final String KEY_NO_SHADOW_OVERWRITE = KEY_PREFIX + "noShadowOverwrite";
    public static final String VARIANT_ONLY_LABEL = "onlyLabel";
    public static final String VARIANT_FULL_SHADOW = "fullShadow";
    public static final String VARIANT_SHADOW = "shadow";
    public static final String VARIANT_NONE = "none";
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
    protected int arc;
    protected int squareArc;

    protected final AbstractButtonLayoutDelegate layoutDelegate = new AbstractButtonLayoutDelegate() {
        @Override
        public Font getFont() {
            return delegate != null ? delegate.getFont().deriveFont(Font.BOLD) : null;
        }
    };

    public static ComponentUI createUI(final JComponent c) {
        return new DarkButtonUI();
    }

    public static boolean chooseAlternativeArc(final Component c) {
        return c instanceof JButton
            && Boolean.TRUE.equals(((JButton) c).getClientProperty(KEY_ALT_ARC));
    }

    public static boolean isLabelButton(final Component c) {
        return c instanceof JButton
            && VARIANT_ONLY_LABEL.equals(((JButton) c).getClientProperty(KEY_VARIANT));
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

    public static boolean isShadowVariant(final Component c) {
        if (isFullShadow(c)) return true;
        if (c instanceof JButton) {
            JButton b = (JButton) c;
            return doConvertToShadow((AbstractButton) c) || VARIANT_SHADOW.equals(b.getClientProperty(KEY_VARIANT));
        }
        return false;
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

    public static boolean isFullShadow(final Component c) {
        return c instanceof JButton
            && VARIANT_FULL_SHADOW.equals(((JButton) c).getClientProperty(KEY_VARIANT));
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

    public static boolean isNoArc(final Component c) {
        return c instanceof JButton
            && Boolean.TRUE.equals(((JButton) c).getClientProperty(KEY_NO_ARC));
    }

    private boolean shouldDrawBackground(final JComponent c) {
        if (isLabelButton(c)) return false;
        AbstractButton button = (AbstractButton) c;
        Border border = c.getBorder();
        return c.isEnabled() && border != null && button.isContentAreaFilled();
    }

    public static boolean isSquare(final Component c) {
        return c instanceof JButton && Boolean.TRUE.equals(((JButton) c).getClientProperty(KEY_SQUARE));
    }

    public static boolean isThin(final Component c) {
        if (c instanceof JButton) {
            boolean isThin = Boolean.TRUE.equals(((JButton) c).getClientProperty(KEY_THIN));
            return isThin || doConvertToShadow((AbstractButton) c);
        }
        return false;
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
        if (doConvertToShadow(b)) {
            int size = Math.min(viewRect.width, viewRect.height);
            viewRect.width = viewRect.height = size;
        }

        textRect.x = textRect.y = textRect.width = textRect.height = 0;
        iconRect.x = iconRect.y = iconRect.width = iconRect.height = 0;
        // layout the text and icon
        return SwingUtilities.layoutCompoundLabel(b, fm, b.getText(), b.getIcon(),
                                                  b.getVerticalAlignment(), b.getHorizontalAlignment(),
                                                  b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
                                                  viewRect, iconRect, textRect,
                                                  b.getText() == null || isIconOnly(b) ? 0 : b.getIconTextGap());
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

    protected static boolean doConvertToShadow(final AbstractButton b) {
        return isIconOnly(b) && convertIconButtonToShadow(b);
    }

    protected Color getBackgroundColor(final JComponent c) {
        boolean defaultButton = isDefaultButton(c);
        AbstractButton b = (AbstractButton) c;
        boolean rollOver = (b.isRolloverEnabled() || doConvertToShadow(b)) && (((JButton) c).getModel().isRollover());
        boolean clicked = b.getModel().isArmed();
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

    protected static boolean convertIconButtonToShadow(final AbstractButton b) {
        return !(b instanceof UIResource)
            && UIManager.getBoolean("Button.convertIconOnlyToShadow")
            && !Boolean.TRUE.equals(b.getClientProperty(KEY_NO_SHADOW_OVERWRITE));
    }

    @Override
    protected void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        LookAndFeel.installProperty(b, PropertyKey.OPAQUE, false);
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


    protected Color getShadowColor(final AbstractButton c) {
        Object colorHover = c.getClientProperty(KEY_HOVER_COLOR);
        Object colorClick = c.getClientProperty(KEY_CLICK_COLOR);
        return c.getModel().isArmed() ? colorClick instanceof Color ? (Color) colorClick : shadowClick
                                      : colorHover instanceof Color ? (Color) colorHover : shadowHover;
    }

    @Override
    public void installUI(final JComponent c) {
        button = (AbstractButton) c;
        super.installUI(c);
    }

    @Override
    protected BasicButtonListener createButtonListener(final AbstractButton b) {
        return new DarkButtonListener(b);
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
            int width = c.getWidth();
            int height = c.getHeight();
            Insets margin = b.getMargin();
            if (margin instanceof UIResource) margin = new Insets(0, 0, 0, 0);
            if (isShadowVariant(c)) {
                if (b.isEnabled() && b.getModel().isRollover()) {
                    GraphicsUtil.setupAAPainting(g2);
                    g.setColor(getShadowColor(b));
                    if (isFullShadow(c)) {
                        g.fillRect(margin.left, margin.top,
                                   width - margin.left - margin.right,
                                   height - margin.top - margin.bottom);
                    } else if (doConvertToShadow(b)) {
                        int size = Math.min(width - margin.left - margin.right,
                                            height - margin.left - margin.right);
                        g.fillRoundRect((width - size) / 2, (height - size) / 2, size, size, arc, arc);
                    } else {
                        g.fillRoundRect(margin.left, margin.top,
                                        width - margin.left - margin.right,
                                        height - margin.top - margin.bottom,
                                        arc, arc);
                    }
                }
            } else {
                g2.setColor(getBackgroundColor(c));
                if (isSquare(c) && !chooseAlternativeArc(c)) {
                    g2.fillRect(borderSize, borderSize, width - 2 * borderSize,
                                height - 2 * borderSize - shadowHeight);
                } else {
                    DarkUIUtil.fillRoundRect((Graphics2D) g, borderSize, borderSize, width - 2 * borderSize,
                                             height - 2 * borderSize - shadowHeight, arc);
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
        if (key.startsWith(KEY_PREFIX)) {
            button.revalidate();
        }
    }
}
