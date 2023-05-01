/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.button;

import java.awt.*;
import java.awt.geom.RoundRectangle2D;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicButtonListener;
import javax.swing.plaf.basic.BasicButtonUI;
import javax.swing.plaf.basic.BasicGraphicsUtils;

import com.github.weisj.darklaf.components.tooltip.ToolTipStyle;
import com.github.weisj.darklaf.delegate.AbstractButtonLayoutDelegate;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.graphics.StringPainter;
import com.github.weisj.darklaf.swingdsl.VisualPaddingListener;
import com.github.weisj.darklaf.ui.togglebutton.ToggleButtonFocusNavigationActions;
import com.github.weisj.darklaf.ui.tooltip.ToolTipConstants;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.util.AlignmentExt;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;
import com.github.weisj.darklaf.util.value.CleanupTask;
import com.github.weisj.darklaf.util.value.WeakShared;

/** @author Jannis Weis */
public class DarkButtonUI extends BasicButtonUI implements ButtonConstants {

    protected static final RoundRectangle2D hitArea = new RoundRectangle2D.Float();
    private static final WeakShared<AbstractButtonLayoutDelegate> sharedLayoutDelegate =
            new WeakShared<>(ButtonLayoutDelegate::new);

    protected AbstractButtonLayoutDelegate layoutDelegate;
    protected boolean isDefaultButton = false;

    protected final Rectangle viewRect = new Rectangle();
    protected final Rectangle textRect = new Rectangle();
    protected final Rectangle iconRect = new Rectangle();
    protected String displayText;

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
    protected VisualPaddingListener visualPaddingListener;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkButtonUI();
    }

    @Override
    public void installUI(final JComponent c) {
        button = (AbstractButton) c;
        layoutDelegate = sharedLayoutDelegate.get();
        super.installUI(c);
    }

    @Override
    protected void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        b.setLayout(createLayout());
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
        if (insets == null) insets = new Insets(0, 0, 0, 0);
        if (thinInsets == null) thinInsets = new Insets(0, 0, 0, 0);
        if (squareThinInsets == null) squareThinInsets = new Insets(0, 0, 0, 0);
        if (squareInsets == null) squareInsets = new Insets(0, 0, 0, 0);
        if (borderlessRectangularInsets == null) borderlessRectangularInsets = new Insets(0, 0, 0, 0);
        updateMargins(b);
    }

    protected LayoutManager createLayout() {
        return new DarkButtonLayout();
    }

    @Override
    protected void installListeners(final AbstractButton b) {
        super.installListeners(b);
        keyboardActions = new ToggleButtonFocusNavigationActions(b);
        keyboardActions.installActions();
        visualPaddingListener = new VisualPaddingListener(b);
        b.addPropertyChangeListener(visualPaddingListener);
    }

    @Override
    protected BasicButtonListener createButtonListener(final AbstractButton b) {
        return new DarkButtonListener<>(b, this);
    }

    @Override
    public void uninstallUI(JComponent c) {
        super.uninstallUI(c);
        layoutDelegate = null;
    }

    @Override
    protected void uninstallDefaults(final AbstractButton b) {
        super.uninstallDefaults(b);
        b.setLayout(null);
    }

    @Override
    protected void uninstallListeners(final AbstractButton b) {
        super.uninstallListeners(b);
        keyboardActions.uninstallActions();
        keyboardActions = null;
        b.removePropertyChangeListener(visualPaddingListener);
        visualPaddingListener = null;
    }

    protected void validateLayout() {
        boolean defButton = ButtonConstants.isDefaultButton(button);
        if (defButton != isDefaultButton) {
            isDefaultButton = defButton;
            button.doLayout();
        }
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        validateLayout();
        GraphicsContext config = new GraphicsContext(g);

        AbstractButton b = (AbstractButton) c;
        try (CleanupTask clean = prepareDelegate(b)) {
            paintButtonBackground(g, c);

            paintIcon(g, b, c);
            config.restoreClip();
            paintText(g, b, displayText);
        }
    }

    protected void paintButtonBackground(final Graphics g, final JComponent c) {
        Graphics2D g2 = (Graphics2D) g;
        AbstractButton b = (AbstractButton) c;
        if (shouldDrawBackground(b)) {
            int arcSize = getArc(c);
            int width = c.getWidth();
            int height = c.getHeight();
            Insets margin = b.getMargin();
            if (margin instanceof UIResource) {
                margin = null;
            }
            if (ButtonConstants.isBorderlessVariant(c)) {
                paintBorderlessBackground(b, g2, arcSize, width, height, margin);
            } else if (b.getBorder() instanceof DarkButtonBorder) {
                paintDarklafBorderBackground(b, g2, arcSize, width, height);
            } else {
                paintDefaultBackground(b, g2, width, height);
            }
        }
    }

    protected void paintDefaultBackground(final AbstractButton b, final Graphics2D g, final int width,
            final int height) {
        Insets ins = b.getInsets();
        g.setColor(getBackgroundColor(b, true));
        PaintUtil.fillRect(g, ins.left, ins.top, width - ins.left - ins.right, height - ins.top - ins.bottom);
    }

    protected void paintDarklafBorderBackground(final AbstractButton c, final Graphics2D g, final int arc,
            final int width, final int height) {
        boolean showShadow = DarkButtonBorder.showDropShadow(c);
        int shadow = showShadow ? shadowHeight : 0;
        int effectiveArc = ButtonConstants.chooseArcWithBorder(c, arc, 0, 0, borderSize);
        AlignmentExt corner = DarkButtonBorder.getCornerFlag(c);
        DarkButtonBorder border = ((DarkButtonBorder) c.getBorder());
        Rectangle bgRect = border.getEffectiveRect(c, width, height, effectiveArc, corner);
        paintDarklafBorderBgImpl(c, g, width, height, shadow, effectiveArc, bgRect);
    }

    protected void paintDarklafBorderBgImpl(final AbstractButton c, final Graphics2D g, final int width,
            final int height, final int shadow, final int effectiveArc, final Rectangle bgRect) {
        DarkButtonBorder border = ((DarkButtonBorder) c.getBorder());

        if (c.isEnabled() && shadow > 0 && PaintUtil.getShadowComposite().getAlpha() != 0) {
            paintShadow(g, shadow, effectiveArc, bgRect);
        }

        Color bg1 = getBackgroundColor(c, false);
        Color bg2 = getBackgroundColor(c, true);

        if (!bg1.equals(bg2)) {
            Rectangle bg2Rect = border.getEffectiveRect(c, width, height, effectiveArc, null);
            g.setColor(bg1);
            paintBackgroundRect(g, effectiveArc, bgRect);
            g.setColor(bg2);
            paintBackgroundRect(g, effectiveArc, bg2Rect);
        } else {
            g.setColor(bg2);
            paintBackgroundRect(g, effectiveArc, bgRect);
        }
    }

    protected void paintShadow(final Graphics2D g, final int shadow, final int effectiveArc, final Rectangle bgRect) {
        g.setColor(shadowColor);
        Composite comp = g.getComposite();
        g.setComposite(PaintUtil.getShadowComposite());
        int stroke = (int) PaintUtil.getStrokeWidth(g);
        paintBackgroundRect(g, effectiveArc * 2, bgRect.x, bgRect.y + shadow + stroke, bgRect.width, bgRect.height);
        g.setComposite(comp);
    }

    protected void paintBackgroundRect(final Graphics2D g2, final int effectiveArc, final Rectangle bgRect) {
        paintBackgroundRect(g2, effectiveArc, bgRect.x, bgRect.y, bgRect.width, bgRect.height);
    }

    protected void paintBackgroundRect(final Graphics2D g2, final int effectiveArc, final int x, final int y,
            final int width, final int height) {
        if (effectiveArc == 0) {
            g2.fillRect(x, y, width, height);
        } else {
            PaintUtil.fillRoundRect(g2, x, y, width, height, effectiveArc, false);
        }
    }

    protected Rectangle backgroundContentRect(final AbstractButton b, final int width, final int height,
            final Insets m) {
        Border border = b.getBorder();
        Insets ins = border != null ? border.getBorderInsets(b) : new Insets(0, 0, 0, 0);
        Insets margin = m;
        if (margin == null) {
            margin = new Insets(0, 0, 0, 0);
        } else {
            // Ensure margins really only affect the size of the background around the content.
            // If the button is larger than expected adjust the margin s.t. the shadow background is
            // only painted in the area around the viewRect specified by the margin.
            Rectangle r = iconRect.union(textRect);
            margin.left = r.x - margin.left;
            margin.right = width - (r.x + r.width + margin.right);
            margin.top = r.y - margin.top;
            margin.bottom = height - (r.y + r.height + margin.bottom);
        }

        int x = Math.max(ins.left, margin.left);
        int y = Math.max(ins.top, margin.top);
        int w = width - x - Math.max(ins.right, margin.right);
        int h = height - y - Math.max(ins.bottom, margin.bottom);
        return new Rectangle(x, y, w, h);
    }

    protected void paintBorderlessBackground(final AbstractButton b, final Graphics2D g, final int arc, final int width,
            final int height, final Insets m) {
        if (isRolloverBorderless(b) || isArmedBorderless(b)) {
            Rectangle backgroundRect = backgroundContentRect(b, width, height, m);

            int x = backgroundRect.x;
            int y = backgroundRect.y;
            int w = backgroundRect.width;
            int h = backgroundRect.height;

            GraphicsUtil.setupStrokePainting(g);
            if (ButtonConstants.isBorderlessRectangular(b)) {
                paintBorderlessRectangularBackgroundIml(b, g, x, y, w, h);
            } else if (ButtonConstants.isBorderless(b)) {
                paintBorderlessBackgroundImpl(b, g, arc, x, y, w, h);
            } else {
                int size = Math.min(w, h);
                paintBorderlessBackgroundImpl(b, g, arc, (width - size) / 2, (height - size) / 2, size, size);
            }
        }
    }

    protected void paintBorderlessRectangularBackgroundIml(final AbstractButton b, final Graphics2D g, final int x,
            final int y, final int w, final int h) {
        g.setColor(getBorderlessBackground(b));
        g.fillRect(x, y, w, h);
    }

    protected void paintBorderlessBackgroundImpl(final AbstractButton b, final Graphics2D g, final int arc, final int x,
            final int y, final int w, final int h) {
        if (!drawOutline) {
            g.setColor(getBorderlessBackground(b));
            g.fillRoundRect(x, y, w, h, arc, arc);
        } else {
            g.setColor(getBorderlessOutline(b));
            PaintUtil.paintLineBorder(g, x, y, w, h, arc);
        }
    }

    public boolean isRolloverBorderless(final AbstractButton b) {
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

    protected Color getForeground(final AbstractButton b) {
        return getForegroundColor(b, isDefaultButton, b.getModel().isEnabled());
    }

    protected Color getForegroundColor(final AbstractButton b, final boolean defaultButton, final boolean enabled) {
        Color fg = b.getForeground();
        if (defaultButton && !ButtonConstants.isBorderlessVariant(b)) {
            fg = defaultForeground;
        }
        if (!enabled) {
            fg = inactiveForeground;
        }
        return PropertyUtil.chooseColor(b.getForeground(), fg);
    }

    protected Color getBackgroundColor(final AbstractButton b, final boolean highlightBackground) {
        boolean defaultButton = isDefaultButton;
        boolean rollOver = highlightBackground && b.isRolloverEnabled() && b.getModel().isRollover();
        boolean clicked = highlightBackground && b.getModel().isArmed();
        boolean selected = highlightBackground && b.getModel().isSelected();
        boolean enabled = b.isEnabled();
        return getBackgroundColor(b, defaultButton, rollOver, clicked, enabled, selected);
    }

    protected Color getBackgroundColor(final AbstractButton b, final boolean defaultButton, final boolean rollOver,
            final boolean clicked, final boolean enabled, final boolean selected) {
        if (enabled) {
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
                    return PropertyUtil.chooseColor(b.getBackground(), background);
                }
            }
        } else {
            return inactiveBackground;
        }
    }

    protected Color getBorderlessBackground(final AbstractButton c) {
        return isArmedBorderless(c)
                ? PropertyUtil.getColor(c, KEY_CLICK_COLOR, borderlessClick)
                : PropertyUtil.getColor(c, KEY_HOVER_COLOR, borderlessHover);
    }

    public boolean isArmedBorderless(final AbstractButton b) {
        return b.getModel().isArmed();
    }

    public Color getBorderlessOutline(final AbstractButton c) {
        return getBorderlessOutline(isArmedBorderless(c));
    }

    public Color getBorderlessOutline(final boolean armed) {
        return armed ? borderlessOutlineClick : borderlessOutlineHover;
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        AbstractButton b = (AbstractButton) c;
        try (CleanupTask clean = prepareDelegate(b)) {
            Dimension dim = BasicGraphicsUtils.getPreferredButtonSize(layoutDelegate, b.getIconTextGap());
            DarkUIUtil.addInsets(dim, b.getMargin());
            if (ButtonConstants.isSquare(b)) {
                int size = Math.max(dim.width, dim.height);
                dim.setSize(size, size);
            }
            return dim;
        }
    }

    protected CleanupTask prepareDelegate(final AbstractButton b) {
        CleanupTask closeable = layoutDelegate.useWithDelegate(b);
        Font f = b.getFont();
        if (ButtonConstants.isDefaultButton(b) && !f.isBold()) {
            layoutDelegate.setFont(f.deriveFont(Font.BOLD));
        } else {
            layoutDelegate.setFont(f);
        }
        return closeable;
    }

    public void updateMargins(final AbstractButton b) {
        Insets margin = b.getMargin();
        if (margin != null && !(margin instanceof UIResource)) return;
        Insets m = getMargins(b);
        b.setMargin(new InsetsUIResource(m.top, m.left, m.bottom, m.right));
    }

    protected Insets getMargins(final AbstractButton b) {
        if (ButtonConstants.isBorderlessRectangular(b)) return borderlessRectangularInsets;
        boolean square = ButtonConstants.isSquare(b);
        return ButtonConstants.isThin(b) ? square ? squareThinInsets : thinInsets : square ? squareInsets : insets;
    }

    @Override
    public boolean contains(final JComponent c, final int x, final int y) {
        if (ButtonConstants.isBorderlessRectangular(c)) {
            return super.contains(c, x, y);
        }
        if (!(x >= 0 && x <= c.getWidth() && y >= 0 && y <= c.getHeight())) return false;
        int bs = c.getBorder() instanceof DarkButtonBorder && !ButtonConstants.isBorderless(c) ? borderSize : 0;
        int arc = getArc(c);
        hitArea.setRoundRect(bs, bs, c.getWidth() - 2 * bs, c.getHeight() - 2 * bs, arc, arc);
        return hitArea.contains(x, y);
    }

    public boolean getDrawOutline(final Component c) {
        return drawOutline && ButtonConstants.isBorderless(c);
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

    public class DarkButtonLayout implements LayoutManager {

        @Override
        public void addLayoutComponent(final String name, final Component comp) {}

        @Override
        public void removeLayoutComponent(final Component comp) {}

        @Override
        public Dimension preferredLayoutSize(final Container parent) {
            // Still managed by BasicButtonUI#getPreferredSize
            return null;
        }

        @Override
        public Dimension minimumLayoutSize(final Container parent) {
            // Still managed by BasicButtonUI#getMinimumSize
            return null;
        }

        @Override
        public void layoutContainer(final Container parent) {
            AbstractButton b = (AbstractButton) parent;
            try (CleanupTask clean = prepareDelegate(b)) {
                FontMetrics fm = b.getFontMetrics(layoutDelegate.getFont());
                displayText = layout(layoutDelegate, b, fm, b.getWidth(), b.getHeight());
            }
        }

        protected String layout(final AbstractButtonLayoutDelegate bl, final AbstractButton b, final FontMetrics fm,
                final int width, final int height) {
            prepareContentRects(b, width, height);
            int horizontalAlignment = getHorizontalAlignment(b);
            int verticalAlignment = getHorizontalAlignment(b);
            int verticalTextPosition = getVerticalTextPosition(b);
            int horizontalTextPosition = getHorizontalTextPosition(b);
            // layout the text and icon
            return SwingUtilities.layoutCompoundLabel(bl, fm, bl.getText(), bl.getIcon(), verticalAlignment,
                    horizontalAlignment, verticalTextPosition, horizontalTextPosition, viewRect, iconRect, textRect,
                    bl.getText() == null || ButtonConstants.isIconOnly(b) ? 0 : bl.getIconTextGap());
        }

        protected int getHorizontalTextPosition(final AbstractButton b) {
            return b.getHorizontalTextPosition();
        }

        protected int getHorizontalAlignment(final AbstractButton b) {
            return b.getHorizontalAlignment();
        }

        protected int getVerticalTextPosition(final AbstractButton b) {
            return b.getVerticalTextPosition();
        }

        protected int getVerticalAlignment(final AbstractButton b) {
            return b.getVerticalAlignment();
        }

        protected void prepareContentRects(final AbstractButton b, final int width, final int height) {
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
        }
    }
}
