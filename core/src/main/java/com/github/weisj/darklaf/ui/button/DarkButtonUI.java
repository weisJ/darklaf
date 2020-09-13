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
package com.github.weisj.darklaf.ui.button;

import java.awt.*;
import java.awt.geom.RoundRectangle2D;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicButtonListener;
import javax.swing.plaf.basic.BasicButtonUI;
import javax.swing.plaf.basic.BasicGraphicsUtils;

import sun.swing.SwingUtilities2;

import com.github.weisj.darklaf.components.tooltip.ToolTipStyle;
import com.github.weisj.darklaf.delegate.AbstractButtonLayoutDelegate;
import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.GraphicsUtil;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.graphics.StringPainter;
import com.github.weisj.darklaf.ui.togglebutton.ToggleButtonFocusNavigationActions;
import com.github.weisj.darklaf.ui.tooltip.ToolTipConstants;
import com.github.weisj.darklaf.util.AlignmentExt;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

/** @author Jannis Weis */
public class DarkButtonUI extends BasicButtonUI implements ButtonConstants {

    protected static final Rectangle viewRect = new Rectangle();
    protected static final Rectangle textRect = new Rectangle();
    protected static final Rectangle iconRect = new Rectangle();
    protected static final RoundRectangle2D hitArea = new RoundRectangle2D.Float();
    protected static final AbstractButtonLayoutDelegate layoutDelegate = new ButtonLayoutDelegate();
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

    protected Insets insets;
    protected Insets thinInsets;
    protected Insets squareInsets;
    protected Insets squareThinInsets;
    protected Insets borderlessRectangularInsets;

    protected AbstractButton button;
    protected int arc;
    protected int altArc;
    protected ToggleButtonFocusNavigationActions keyboardActions;

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
        PropertyUtil.installProperty(b, ToolTipConstants.KEY_STYLE,
                ToolTipStyle.parse(UIManager.get("Button.toolTipStyle")));
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
        altArc = UIManager.getInt("Button.altArc");
        drawOutline = UIManager.getBoolean("Button.borderless.drawOutline");
        insets = UIManager.getInsets("Button.borderInsets");
        thinInsets = UIManager.getInsets("Button.thinBorderInsets");
        squareInsets = UIManager.getInsets("Button.squareBorderInsets");
        squareThinInsets = UIManager.getInsets("Button.squareThinBorderInsets");
        borderlessRectangularInsets = UIManager.getInsets("Button.borderlessRectangularInsets");
        if (insets == null)
            insets = new Insets(0, 0, 0, 0);
        if (thinInsets == null)
            thinInsets = new Insets(0, 0, 0, 0);
        if (squareThinInsets == null)
            squareThinInsets = new Insets(0, 0, 0, 0);
        if (squareInsets == null)
            squareInsets = new Insets(0, 0, 0, 0);
        if (borderlessRectangularInsets == null)
            borderlessRectangularInsets = new Insets(0, 0, 0, 0);
        updateMargins(b);
    }

    @Override
    protected void installListeners(final AbstractButton b) {
        super.installListeners(b);
        keyboardActions = new ToggleButtonFocusNavigationActions(b);
        keyboardActions.installActions();
    }

    @Override
    protected BasicButtonListener createButtonListener(final AbstractButton b) {
        return new DarkButtonListener(b, this);
    }

    @Override
    protected void uninstallListeners(final AbstractButton b) {
        super.uninstallListeners(b);
        keyboardActions.uninstallActions();
        keyboardActions = null;
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        GraphicsContext config = new GraphicsContext(g);
        AbstractButton b = (AbstractButton) c;

        prepareDelegate(b);
        String text = layout(layoutDelegate, b, SwingUtilities2.getFontMetrics(b, g, layoutDelegate.getFont()),
                b.getWidth(), b.getHeight());
        paintButtonBackground(g, c);

        paintIcon(g, b, c);
        config.restoreClip();
        paintText(g, b, text);
    }

    protected void paintButtonBackground(final Graphics g, final JComponent c) {
        Graphics2D g2 = (Graphics2D) g;
        AbstractButton b = (AbstractButton) c;
        if (shouldDrawBackground(b)) {
            int arc = getArc(c);
            int width = c.getWidth();
            int height = c.getHeight();
            Insets margin = b.getMargin();
            if (margin instanceof UIResource) {
                margin = null;
            }
            if (ButtonConstants.isBorderlessVariant(c)) {
                paintBorderlessBackground(b, g2, arc, width, height, margin);
            } else if (b.getBorder() instanceof DarkButtonBorder) {
                paintDarklafBorderBackground(b, g2, arc, width, height);
            } else {
                paintDefaultBackgroud(b, g2, width, height);
            }
        }
    }

    protected void paintDefaultBackgroud(final AbstractButton b, final Graphics2D g, final int width,
            final int height) {
        Insets ins = b.getInsets();
        g.setColor(getBackgroundColor(b));
        PaintUtil.fillRect(g, ins.left, ins.top, width - ins.left - ins.right, height - ins.top - ins.bottom);
    }

    protected void paintDarklafBorderBackground(final AbstractButton c, final Graphics2D g, final int arc,
            final int width, final int height) {
        boolean showShadow = DarkButtonBorder.showDropShadow(c);
        int shadow = showShadow ? shadowHeight : 0;
        int effectiveArc = ButtonConstants.chooseArcWithBorder(c, arc, 0, 0, borderSize);
        AlignmentExt corner = DarkButtonBorder.getCornerFlag(c);

        Rectangle bgRect = getEffectiveRect(width, height, -(effectiveArc + 1), corner);
        if (c.isEnabled() && showShadow && PaintUtil.getShadowComposite().getAlpha() != 0) {
            g.setColor(shadowColor);
            Composite comp = g.getComposite();
            g.setComposite(PaintUtil.getShadowComposite());
            int stroke = (int) PaintUtil.getStrokeWidth(g);
            paintBackgroundRect(g, effectiveArc * 2, bgRect.x, bgRect.y + shadow + stroke, bgRect.width, bgRect.height);
            g.setComposite(comp);
        }

        g.setColor(getBackgroundColor(c));
        paintBackgroundRect(g, effectiveArc, bgRect);
    }

    private void paintBackgroundRect(final Graphics2D g2, final int effectiveArc, final Rectangle bgRect) {
        paintBackgroundRect(g2, effectiveArc, bgRect.x, bgRect.y, bgRect.width, bgRect.height);
    }

    private void paintBackgroundRect(final Graphics2D g2, final int effectiveArc, final int x, final int y,
            final int width, final int height) {
        if (effectiveArc == 0) {
            g2.fillRect(x, y, width, height);
        } else {
            PaintUtil.fillRoundRect(g2, x, y, width, height, effectiveArc, false);
        }
    }

    protected Rectangle getEffectiveRect(final int width, final int height, final int adjustment,
            final AlignmentExt corner) {
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

    protected void paintBorderlessBackground(final AbstractButton b, final Graphics2D g, final int arc, final int width,
            final int height, final Insets m) {
        if (isRolloverBorderless(b)) {
            Insets ins = b.getInsets();
            Insets margin = m;
            if (margin == null) {
                margin = new Insets(0, 0, 0, 0);
            } else {
                // Ensure margins really only affect the size of the background around the content.
                // If the button is larger than expected adjust the margin s.t. the shadow background is
                // only painted in the area around the viewRect specified by the margin.
                Rectangle r = iconRect.union(textRect);
                int prefWidth = r.width + margin.right + margin.left + ins.left + ins.right;
                int prefHeight = r.height + margin.top + margin.bottom + ins.top + ins.bottom;
                margin = new Insets(margin.top, margin.left, margin.bottom, margin.right);
                if (width > prefWidth) {
                    margin.left = r.x - margin.left;
                    margin.right = width - (r.x + r.width + margin.right);
                }
                if (height > prefHeight) {
                    margin.top = r.y - margin.top;
                    margin.bottom = height - (r.y + r.height + margin.bottom);
                }
            }

            int x = Math.max(ins.left, margin.left);
            int y = Math.max(ins.top, margin.top);
            int w = width - x - Math.max(ins.right, margin.right);
            int h = height - y - Math.max(ins.bottom, margin.bottom);

            GraphicsUtil.setupAAPainting(g);
            g.setColor(getBorderlessBackground(b));
            if (ButtonConstants.isBorderlessRectangular(b)) {
                g.fillRect(x, y, w, h);
            } else if (ButtonConstants.doConvertToBorderless(b)) {
                int size = Math.min(w, h);
                if (!drawOutline) {
                    g.fillRoundRect((width - size) / 2, (height - size) / 2, size, size, arc, arc);
                } else {
                    g.setColor(getBorderlessOutline(b));
                    PaintUtil.paintLineBorder(g, (width - size) / 2.0f, (height - size) / 2.0f, size, size, arc);
                }
            } else {
                if (!drawOutline) {
                    g.fillRoundRect(x, y, w, h, arc, arc);
                } else {
                    g.setColor(getBorderlessOutline(b));
                    PaintUtil.paintLineBorder(g, x, y, w, h, arc);
                }
            }
        }
    }

    protected boolean isRolloverBorderless(final AbstractButton b) {
        return b.isEnabled() && b.getModel().isRollover();
    }

    protected void paintText(final Graphics g, final AbstractButton b, final String text) {
        ButtonModel model = b.getModel();
        g.setColor(getForeground(b));
        int mnemonicIndex = b.getDisplayedMnemonicIndex();
        if (!model.isEnabled()) {
            mnemonicIndex = -1;
        }
        StringPainter.drawStringUnderlineCharAt(g, b, text, mnemonicIndex, textRect, layoutDelegate.getFont());
    }

    protected void paintIcon(final Graphics g, final AbstractButton b, final JComponent c) {
        if (b.getIcon() != null) {
            g.clipRect(iconRect.x, iconRect.y, iconRect.width, iconRect.height);
            paintIcon(g, c, iconRect);
        }
    }

    protected void repaintNeighbours() {
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_LEFT_NEIGHBOUR, button));
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_TOP_NEIGHBOUR, button));
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_TOP_RIGHT_NEIGHBOUR, button));
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_TOP_LEFT_NEIGHBOUR, button));
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_RIGHT_NEIGHBOUR, button));
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_BOTTOM_NEIGHBOUR, button));
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_BOTTOM_RIGHT_NEIGHBOUR, button));
        DarkUIUtil.repaint(ButtonConstants.getNeighbour(KEY_BOTTOM_LEFT_NEIGHBOUR, button));
    }

    protected boolean shouldDrawBackground(final AbstractButton c) {
        return button.isContentAreaFilled();
    }

    protected int getArc(final Component c) {
        return ButtonConstants.chooseArcWithBorder(c, arc, 0, altArc, borderSize);
    }

    protected Color getForeground(final AbstractButton button) {
        Color fg = button.getForeground();
        if (fg instanceof UIResource && ButtonConstants.isDefaultButton(button)
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
        return isArmedBorderless(c) ? PropertyUtil.getColor(c, KEY_CLICK_COLOR, borderlessClick)
                : PropertyUtil.getColor(c, KEY_HOVER_COLOR, borderlessHover);
    }

    protected boolean isArmedBorderless(final AbstractButton b) {
        return b.getModel().isArmed();
    }

    protected Color getBorderlessOutline(final AbstractButton c) {
        boolean armed = c.getModel().isArmed();
        return armed ? borderlessOutlineClick : borderlessOutlineHover;
    }

    protected String layout(final AbstractButtonLayoutDelegate bl, final AbstractButton b, final FontMetrics fm,
            final int width, final int height) {
        Insets i = DarkUIUtil.addInsets(b.getInsets(), b.getMargin());

        AlignmentExt corner = DarkButtonBorder.getCornerFlag(b);
        if (corner != null) {
            Insets insetMask = new Insets(borderSize, borderSize, borderSize, borderSize);
            insetMask = corner.maskInsetsInverted(insetMask, 0);
            i.left -= insetMask.left;
            i.right -= insetMask.right;
            i.top -= insetMask.top;
            i.bottom -= insetMask.bottom;
        }

        viewRect.setRect(0, 0, width, height);
        DarkUIUtil.applyInsets(viewRect, i);

        textRect.x = textRect.y = textRect.width = textRect.height = 0;
        iconRect.x = iconRect.y = iconRect.width = iconRect.height = 0;
        // layout the text and icon
        return SwingUtilities.layoutCompoundLabel(bl, fm, bl.getText(), bl.getIcon(), bl.getVerticalAlignment(),
                bl.getHorizontalAlignment(), bl.getVerticalTextPosition(), bl.getHorizontalTextPosition(), viewRect,
                iconRect, textRect, bl.getText() == null || ButtonConstants.isIconOnly(b) ? 0 : bl.getIconTextGap());
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        AbstractButton b = (AbstractButton) c;
        prepareDelegate(b);
        Dimension dim = BasicGraphicsUtils.getPreferredButtonSize(layoutDelegate, b.getIconTextGap());
        DarkUIUtil.addInsets(dim, b.getMargin());
        if (ButtonConstants.isSquare(b)) {
            int size = Math.max(dim.width, dim.height);
            dim.setSize(size, size);
        }
        return dim;
    }

    protected void prepareDelegate(final AbstractButton b) {
        layoutDelegate.setDelegate(b);
        Font f = b.getFont();
        if (ButtonConstants.isDefaultButton(b) && !f.isBold()) {
            layoutDelegate.setFont(f.deriveFont(Font.BOLD));
        } else {
            layoutDelegate.setFont(f);
        }
    }

    protected void updateMargins(final AbstractButton b) {
        Insets margin = b.getMargin();
        if (margin != null && !(margin instanceof UIResource))
            return;
        Insets m = getMargins(b);
        b.setMargin(new InsetsUIResource(m.top, m.left, m.bottom, m.right));
    }

    private Insets getMargins(final AbstractButton b) {
        if (ButtonConstants.isBorderlessRectangular(b))
            return borderlessRectangularInsets;
        boolean square = ButtonConstants.isSquare(b);
        return ButtonConstants.isThin(b) ? square ? squareThinInsets : thinInsets : square ? squareInsets : insets;
    }

    @Override
    public boolean contains(final JComponent c, final int x, final int y) {
        if (ButtonConstants.isBorderlessRectangular(c)) {
            return super.contains(c, x, y);
        }
        if (!(x >= 0 && x <= c.getWidth() && y >= 0 && y <= c.getHeight()))
            return false;
        int bs = c.getBorder() instanceof DarkButtonBorder && !ButtonConstants.isBorderless(c) ? borderSize : 0;
        int arc = getArc(c);
        hitArea.setRoundRect(bs, bs, c.getWidth() - 2 * bs, c.getHeight() - 2 * bs, arc, arc);
        return hitArea.contains(x, y);
    }

    protected static class ButtonLayoutDelegate extends AbstractButtonLayoutDelegate {
        protected Font font;

        @Override
        public void setFont(final Font font) {
            this.font = font;
        }

        @Override
        public Font getFont() {
            return font;
        }
    }
}
