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
 *
 */
package com.github.weisj.darklaf.ui.button;

import java.awt.*;
import java.awt.event.KeyListener;
import java.awt.geom.RoundRectangle2D;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicButtonListener;
import javax.swing.plaf.basic.BasicButtonUI;
import javax.swing.plaf.basic.BasicGraphicsUtils;

import sun.swing.SwingUtilities2;

import com.github.weisj.darklaf.delegate.AbstractButtonLayoutDelegate;
import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.GraphicsUtil;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.togglebutton.DarkToggleButtonKeyHandler;
import com.github.weisj.darklaf.ui.togglebutton.ToggleButtonFocusNavigationActions;
import com.github.weisj.darklaf.ui.tooltip.ToolTipConstants;
import com.github.weisj.darklaf.util.AlignmentExt;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkButtonUI extends BasicButtonUI implements ButtonConstants {

    protected static final Rectangle viewRect = new Rectangle();
    protected static final Rectangle textRect = new Rectangle();
    protected static final Rectangle iconRect = new Rectangle();
    protected int borderSize;
    protected int shadowHeight;
    protected boolean drawOutline;
    protected Color inactiveForeground;
    protected Color defaultForeground;
    protected Color defaultBackground;
    protected Color defaultHoverBackground;
    protected Color defaultClickBackground;
    protected Color background;
    protected Color hoverBackground;
    protected Color clickBackground;
    protected Color inactiveBackground;
    protected Color borderlessHover;
    protected Color borderlessClick;
    protected Color borderlessOutlineHover;
    protected Color borderlessOutlineClick;
    protected Color shadowColor;
    protected AbstractButton button;
    protected int arc;
    protected int squareArc;
    protected KeyListener keyListener;

    protected final AbstractButtonLayoutDelegate layoutDelegate = new AbstractButtonLayoutDelegate() {
        @Override
        public Font getFont() {
            return delegate != null ? delegate.getFont().deriveFont(Font.BOLD) : null;
        }
    };

    public static ComponentUI createUI(final JComponent c) {
        return new DarkButtonUI();
    }

    @Override
    public void installUI(final JComponent c) {
        button = (AbstractButton) c;
        super.installUI(c);
    }

    @Override
    protected void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        PropertyUtil.installProperty(b, ToolTipConstants.KEY_STYLE, ToolTipConstants.VARIANT_BALLOON);
        LookAndFeel.installProperty(b, PropertyKey.OPAQUE, false);
        borderSize = UIManager.getInt("Button.borderThickness");
        shadowHeight = UIManager.getInt("Button.shadowHeight");
        shadowColor = UIManager.getColor("Button.shadow");
        inactiveForeground = UIManager.getColor("Button.disabledText");
        defaultForeground = UIManager.getColor("Button.selectedButtonForeground");
        defaultBackground = UIManager.getColor("Button.defaultFillColor");
        defaultHoverBackground = UIManager.getColor("Button.defaultFillColorRollOver");
        defaultClickBackground = UIManager.getColor("Button.defaultFillColorClick");
        background = UIManager.getColor("Button.activeFillColor");
        hoverBackground = UIManager.getColor("Button.activeFillColorRollOver");
        clickBackground = UIManager.getColor("Button.activeFillColorClick");
        inactiveBackground = UIManager.getColor("Button.inactiveFillColor");
        borderlessHover = UIManager.getColor("Button.borderless.hover");
        borderlessClick = UIManager.getColor("Button.borderless.click");
        borderlessOutlineHover = UIManager.getColor("Button.borderless.outline.hover");
        borderlessOutlineClick = UIManager.getColor("Button.borderless.outline.click");
        arc = UIManager.getInt("Button.arc");
        squareArc = UIManager.getInt("Button.squareArc");
        drawOutline = UIManager.getBoolean("Button.borderless.drawOutline");
    }

    @Override
    protected void installListeners(final AbstractButton b) {
        super.installListeners(b);
        keyListener = createKeyListener(b);
        b.addKeyListener(keyListener);
        ToggleButtonFocusNavigationActions.installActions(b);
    }

    protected KeyListener createKeyListener(final AbstractButton button) {
        return new DarkToggleButtonKeyHandler();
    }

    @Override
    protected BasicButtonListener createButtonListener(final AbstractButton b) {
        return new DarkButtonListener(b, this);
    }

    @Override
    protected void uninstallListeners(final AbstractButton b) {
        super.uninstallListeners(b);
        b.removeKeyListener(keyListener);
        keyListener = null;
        ToggleButtonFocusNavigationActions.uninstallActions(b);
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        GraphicsContext config = new GraphicsContext(g);
        AbstractButton b = (AbstractButton) c;
        paintButtonBackground(g, c);

        Font font = g.getFont();
        if (ButtonConstants.isDefaultButton(b) && !font.isBold()) {
            g.setFont(font.deriveFont(Font.BOLD));
        } else if (font.isBold()) {
            g.setFont(font.deriveFont(Font.PLAIN));
        }

        String text = layout(b, c, SwingUtilities2.getFontMetrics(b, g), b.getWidth(), b.getHeight());

        paintIcon(g, b, c);
        config.restoreClip();
        paintText(g, b, text);
    }

    protected void paintButtonBackground(final Graphics g, final JComponent c) {
        Graphics2D g2 = (Graphics2D) g;
        if (shouldDrawBackground(c)) {
            AbstractButton b = (AbstractButton) c;
            int arc = getArc(c);
            int width = c.getWidth();
            int height = c.getHeight();
            Insets margin = b.getMargin();
            if (margin instanceof UIResource) margin = new Insets(0, 0, 0, 0);
            if (ButtonConstants.isBorderlessVariant(c)) {
                paintBorderlessBackground(b, g2, arc, width, height, margin);
            } else {
                paintDefaultBackground(b, g2, arc, width, height);
            }
        }
    }

    protected void paintDefaultBackground(final AbstractButton c, final Graphics2D g,
                                          final int arc, final int width, final int height) {
        boolean showShadow = DarkButtonBorder.showDropShadow(c);
        int shadow = showShadow ? shadowHeight : 0;
        int effectiveArc = ButtonConstants.isSquare(c) && !ButtonConstants.chooseAlternativeArc(c) ? 0 : arc;
        AlignmentExt corner = DarkButtonBorder.getCornerFlag(c);
        boolean focus = c.hasFocus() && c.isFocusPainted();

        Rectangle bgRect = getEffectiveRect(width, height, c, -(effectiveArc + 1), corner, focus);
        if (c.isEnabled() && showShadow && PaintUtil.getShadowComposite().getAlpha() != 0) {
            g.setColor(shadowColor);
            Composite comp = g.getComposite();
            g.setComposite(PaintUtil.getShadowComposite());
            int sh = Math.max(shadow, 2 * effectiveArc);
            paintBackgroundRect(g, effectiveArc, bgRect.x, bgRect.y + bgRect.height + shadow - sh,
                                bgRect.width, sh, false);
            g.setComposite(comp);
        }

        g.setColor(getBackgroundColor(c));
        paintBackgroundRect(g, effectiveArc, bgRect, true);
    }

    private void paintBackgroundRect(final Graphics2D g2, final int effectiveArc, final Rectangle bgRect,
                                     final boolean respectBorder) {
        paintBackgroundRect(g2, effectiveArc, bgRect.x, bgRect.y, bgRect.width, bgRect.height, respectBorder);
    }

    private void paintBackgroundRect(final Graphics2D g2, final int effectiveArc,
                                     final int x, final int y, final int width, final int height,
                                     final boolean respectBorder) {
        if (effectiveArc == 0) {
            g2.fillRect(x, y, width, height);
        } else {
            PaintUtil.fillRoundRect(g2, x, y, width, height, effectiveArc, respectBorder);
        }
    }

    protected Rectangle getEffectiveRect(final int width, final int height, final AbstractButton c,
                                         final int adjustment, final AlignmentExt corner, final boolean focus) {
        Insets insetMask = new Insets(borderSize, borderSize, Math.max(borderSize, shadowHeight), borderSize);
        if (corner != null) {
            insetMask = corner.maskInsets(insetMask, adjustment);
        }
        int bx = insetMask.left;
        int by = insetMask.top;
        int bw = width - insetMask.left - insetMask.right;
        int bh = height - insetMask.top - insetMask.bottom;
        return new Rectangle(bx, by, bw, bh);
    }

    protected void paintBorderlessBackground(final AbstractButton b, final Graphics2D g, final int arc,
                                             final int width, final int height, final Insets margin) {
        if (b.isEnabled() && b.getModel().isRollover()) {
            GraphicsUtil.setupAAPainting(g);
            g.setColor(getBorderlessBackground(b));
            if (ButtonConstants.isBorderlessRectangular(b)) {
                g.fillRect(margin.left, margin.top, width - margin.left - margin.right,
                           height - margin.top - margin.bottom);
                PaintUtil.drawRect(g, margin.left, margin.top, width - margin.left - margin.right,
                                   height - margin.top - margin.bottom, 1);
            } else if (ButtonConstants.doConvertToBorderless(b)) {
                int size = Math.min(width - margin.left - margin.right,
                                    height - margin.left - margin.right);
                if (!drawOutline) {
                    g.fillRoundRect((width - size) / 2 + 2, (height - size) / 2 + 2,
                                    size - 4, size - 4, arc, arc);
                } else {
                    g.setColor(getBorderlessOutline(b));
                    g.drawRoundRect((width - size) / 2 + 2, (height - size) / 2 + 2,
                                    size - 4, size - 4, arc, arc);
                }
            } else {
                if (!drawOutline) {
                    g.fillRoundRect(margin.left + 2, margin.top + 2,
                                    width - margin.left - margin.right - 4,
                                    height - margin.top - margin.bottom - 4,
                                    arc, arc);
                } else {
                    g.setColor(getBorderlessOutline(b));
                    g.drawRoundRect(margin.left + 2, margin.top + 2,
                                    width - margin.left - margin.right - 4,
                                    height - margin.top - margin.bottom - 4,
                                    arc, arc);
                }
            }
        }
    }

    @Override
    protected void paintText(final Graphics g, final JComponent c,
                             final Rectangle textRect, final String text) {
        AbstractButton button = (AbstractButton) c;
        ButtonModel model = button.getModel();
        g.setColor(getForeground(button));
        int mnemonicIndex = button.getDisplayedMnemonicIndex();
        if (!model.isEnabled()) {
            mnemonicIndex = -1;
        }
        SwingUtilities2.drawStringUnderlineCharAt(c, g, text, mnemonicIndex,
                                                  textRect.x + getTextShiftOffset(),
                                                  textRect.y + getTextShiftOffset());
    }

    protected void paintText(final Graphics g, final AbstractButton b, final String text) {
        PaintUtil.drawString(g, b, text, textRect, SwingUtilities2.getFontMetrics(b, g), this::paintText);
    }

    protected void paintIcon(final Graphics g, final AbstractButton b, final JComponent c) {
        if (b.getIcon() != null) {
            g.setClip(iconRect);
            paintIcon(g, c, iconRect);
        }
    }

    protected void repaintNeighbours() {
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_LEFT_NEIGHBOUR, button));
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_TOP_NEIGHBOUR, button));
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_RIGHT_NEIGHBOUR, button));
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_BOTTOM_NEIGHBOUR, button));
    }

    protected boolean shouldDrawBackground(final JComponent c) {
        if (ButtonConstants.isLabelButton(c)) return false;
        AbstractButton button = (AbstractButton) c;
        Border border = c.getBorder();
        return c.isEnabled() && border != null && button.isContentAreaFilled();
    }

    protected int getArc(final Component c) {
        if (ButtonConstants.isNoArc(c)) return 0;
        boolean square = ButtonConstants.isSquare(c);
        boolean alt = ButtonConstants.chooseAlternativeArc(c);
        return square ? alt ? arc : squareArc : alt ? squareArc : arc;
    }

    protected Color getForeground(final AbstractButton button) {
        Color fg = button.getForeground();
        if (fg instanceof UIResource
            && ButtonConstants.isDefaultButton(button)
            && !ButtonConstants.isBorderlessVariant(button)) {
            fg = defaultForeground;
        }
        if (fg instanceof UIResource && !button.getModel().isEnabled()) {
            fg = inactiveForeground;
        }
        return fg;
    }

    protected Color getBackgroundColor(final JComponent c) {
        boolean defaultButton = ButtonConstants.isDefaultButton(c);
        AbstractButton b = (AbstractButton) c;
        boolean rollOver = b.isRolloverEnabled() && b.getModel().isRollover();
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

    protected Color getBorderlessBackground(final AbstractButton c) {
        boolean armed = c.getModel().isArmed();
        return armed ? PropertyUtil.getColor(c, KEY_CLICK_COLOR, borderlessClick)
                : PropertyUtil.getColor(c, KEY_HOVER_COLOR, borderlessHover);
    }

    protected Color getBorderlessOutline(final AbstractButton c) {
        boolean armed = c.getModel().isArmed();
        return armed ? borderlessOutlineClick : borderlessOutlineHover;
    }

    protected String layout(final AbstractButton b, final JComponent c, final FontMetrics fm,
                            final int width, final int height) {
        Insets i = b.getInsets();
        if (!ButtonConstants.isBorderlessVariant(b)) {
            i = new Insets(i.top, borderSize, i.bottom, borderSize);
        }

        AlignmentExt corner = DarkButtonBorder.getCornerFlag(c);
        if (corner != null) {
            Insets insetMask = new Insets(borderSize, borderSize, borderSize, borderSize);
            insetMask = corner.maskInsetsInverted(insetMask, 0);
            i.left -= insetMask.left;
            i.right -= insetMask.right;
            i.top -= insetMask.top;
            i.bottom -= insetMask.bottom;
        }

        viewRect.x = i.left;
        viewRect.y = i.top;
        viewRect.width = width - (i.right + i.left);
        viewRect.height = height - (i.bottom + i.top);

        textRect.x = textRect.y = textRect.width = textRect.height = 0;
        iconRect.x = iconRect.y = iconRect.width = iconRect.height = 0;
        // layout the text and icon
        return SwingUtilities.layoutCompoundLabel(b, fm, b.getText(), b.getIcon(),
                                                  b.getVerticalAlignment(), b.getHorizontalAlignment(),
                                                  b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
                                                  viewRect, iconRect, textRect,
                                                  b.getText() == null || ButtonConstants.isIconOnly(b)
                                                          ? 0
                                                          : b.getIconTextGap());
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        AbstractButton b = (AbstractButton) c;
        layoutDelegate.setDelegate(b);
        return BasicGraphicsUtils.getPreferredButtonSize(layoutDelegate, b.getIconTextGap());
    }

    @Override
    public boolean contains(final JComponent c, final int x, final int y) {
        if (ButtonConstants.isLabelButton(c)) {
            return super.contains(c, x, y);
        }
        if (!(x >= 0 && x <= c.getWidth() && y >= 0 && y <= c.getHeight())) return false;
        int bs = borderSize;
        int arc = getArc(c);
        return new RoundRectangle2D.Float(bs, bs, c.getWidth() - 2 * bs, c.getWidth() - 2 * bs,
                                          arc, arc).contains(x, y);
    }
}
