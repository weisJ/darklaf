package com.weis.darklaf.util;

import com.weis.darklaf.decorators.CellRenderer;
import com.weis.darklaf.ui.menu.DarkPopupMenuUI;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import sun.awt.SunToolkit;

import javax.swing.FocusManager;
import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.table.TableCellRenderer;
import javax.swing.tree.TreeCellRenderer;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Path2D;
import java.awt.geom.RoundRectangle2D;

public final class DarkUIUtil {

    public static final Color TRANSPARENT_COLOR = new Color(0, 0, 0, 0);
    public final static AlphaComposite ALPHA_COMPOSITE = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.6f);
    public final static AlphaComposite ALPHA_COMPOSITE_2 = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.8f);
    public final static AlphaComposite SHADOW_COMPOSITE = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.1f);
    public static final boolean USE_QUARTZ = "true".equals(System.getProperty("apple.awt.graphics.UseQuartz"));

    private static Color getErrorGlow() {
        return UIManager.getColor("glowError");
    }

    private static Color getErrorFocusGlow() {
        return UIManager.getColor("glowFocusError");
    }

    private static Color getFocusGlow() {
        return UIManager.getColor("glowFocus");
    }

    private static Color getWarningGlow() {
        return UIManager.getColor("glowWarning");
    }

    public static void paintOutlineBorder(final Graphics2D g, final int width, final int height, final float arc,
                                          final boolean symmetric, final boolean hasFocus, final Outline type) {
        type.setGraphicsColor(g, hasFocus);
        doPaint(g, width, height, arc, symmetric);
    }

    @SuppressWarnings("SuspiciousNameCombination")
    private static void doPaint(@NotNull final Graphics2D g, final int width, final int height, final float arc,
                                final boolean symmetric) {
        var context = GraphicsUtil.setupStrokePainting(g);
        float bw = 2f;
        float lw = 1f;

        float outerArc = arc > 0 ? arc + bw - 2f : bw;
        float rightOuterArc = symmetric ? outerArc : 6f;
        Path2D outerRect = new Path2D.Float(Path2D.WIND_EVEN_ODD);
        outerRect.moveTo(width - rightOuterArc, 0);
        outerRect.quadTo(width, 0, width, rightOuterArc);
        outerRect.lineTo(width, height - rightOuterArc);
        outerRect.quadTo(width, height, width - rightOuterArc, height);
        outerRect.lineTo(outerArc, height);
        outerRect.quadTo(0, height, 0, height - outerArc);
        outerRect.lineTo(0, outerArc);
        outerRect.quadTo(0, 0, outerArc, 0);
        outerRect.closePath();

        bw += lw;
        float rightInnerArc = symmetric ? outerArc : 7f;
        Path2D innerRect = new Path2D.Float(Path2D.WIND_EVEN_ODD);
        innerRect.moveTo(width - rightInnerArc, bw);
        innerRect.quadTo(width - bw, bw, width - bw, rightInnerArc);
        innerRect.lineTo(width - bw, height - rightInnerArc);
        innerRect.quadTo(width - bw, height - bw, width - rightInnerArc, height - bw);
        innerRect.lineTo(outerArc, height - bw);
        innerRect.quadTo(bw, height - bw, bw, height - outerArc);
        innerRect.lineTo(bw, outerArc);
        innerRect.quadTo(bw, bw, outerArc, bw);
        innerRect.closePath();

        Path2D path = new Path2D.Float(Path2D.WIND_EVEN_ODD);
        path.append(outerRect, false);
        path.append(innerRect, false);
        g.fill(path);
        context.restore();
    }

    public static void paintFocusBorder(final Graphics2D g, final int width, final int height, final float arc,
                                        final boolean symmetric) {
        Outline.focus.setGraphicsColor(g, true);
        doPaint(g, width, height, arc, symmetric);
    }

    public static void paintFocusOval(final Graphics2D g, final int x, final int y, final int width, final int height) {
        paintFocusOval(g, (float) x, (float) y, (float) width, (float) height);
    }

    public static void paintFocusOval(final Graphics2D g, final float x, final float y,
                                      final float width, final float height) {
        Outline.focus.setGraphicsColor(g, true);

        float blw = 2f + 1f;
        Path2D shape = new Path2D.Float(Path2D.WIND_EVEN_ODD);
        shape.append(new Ellipse2D.Float(x - blw, y - blw, width + blw * 2, height + blw * 2), false);
        shape.append(new Ellipse2D.Float(x, y, width, height), false);
        g.fill(shape);
    }

    public static void paintLineBorder(final Graphics2D g, final float x, final float y,
                                       final float width, final float height, final int arc, final boolean growByLW) {
        float lw = 0.5f;
        float adj = growByLW ? lw : 0;
        var config = GraphicsUtil.setupStrokePainting(g);
        Path2D border = new Path2D.Float(Path2D.WIND_EVEN_ODD);
        border.append(new RoundRectangle2D.Float(x - adj, y - adj, width + 2 * adj, height + 2 * adj,
                                                 arc + lw, arc + lw), false);
        border.append(new RoundRectangle2D.Float(x + 2 * lw - adj, y + 2 * lw - adj,
                                                 width - 4 * lw + 2 * adj, height - 4 * lw + 2 * adj,
                                                 arc, arc), false);
        g.fill(border);
        config.restore();
    }

    @NotNull
    public static Color blendColors(@NotNull final Color color1, @NotNull final Color color2, final double percent) {
        double inverse_percent = 1.0 - percent;
        int redPart = (int) (color1.getRed() * percent + color2.getRed() * inverse_percent);
        int greenPart = (int) (color1.getGreen() * percent + color2.getGreen() * inverse_percent);
        int bluePart = (int) (color1.getBlue() * percent + color2.getBlue() * inverse_percent);
        return new Color(redPart, greenPart, bluePart);
    }

    public static void drawRect(final Graphics g, final int x, final int y, final int width, final int height,
                                final int thickness) {
        g.fillRect(x, y, width, thickness);
        g.fillRect(x, y, thickness, height);
        g.fillRect(x + width - thickness, y, thickness, height);
        g.fillRect(x, y + height - thickness, width, thickness);
    }

    public static void applyInsets(final Rectangle rect, final Insets insets) {
        if (insets != null && rect != null) {
            rect.x += insets.left;
            rect.y += insets.top;
            rect.width -= (insets.right + rect.x);
            rect.height -= (insets.bottom + rect.y);
        }
    }

    public static boolean hasFocus(final Component c) {
        if (c instanceof Window) {
            return hasFocus(c);
        }
        final Component owner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
        final Component owner2 = FocusManager.getCurrentManager().getFocusOwner();
        return (owner != null && SwingUtilities.isDescendingFrom(owner, c))
                || (owner2 != null && SwingUtilities.isDescendingFrom(owner2, c));
    }

    public static boolean hasFocus(final Window w) {
        var owner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
        if (owner == null) return false;
        return SwingUtilities.getWindowAncestor(owner) == w;
    }

    public static Color getTreeTextBackground() {
        return UIManager.getColor("Tree.textBackground");
    }

    public static int getFocusAcceleratorKeyMask() {
        Toolkit tk = Toolkit.getDefaultToolkit();
        if (tk instanceof SunToolkit) {
            return ((SunToolkit) tk).getFocusAcceleratorKeyMask();
        }
        return ActionEvent.ALT_MASK;
    }

    @Nullable
    @Contract(pure = true)
    public static Object getUIOfType(final ComponentUI ui, @NotNull final Class<?> klass) {
        if (klass.isInstance(ui)) {
            return ui;
        }
        return null;
    }

    public static void doNotCancelPopupSetup(@NotNull final JComponent component) {
        component.putClientProperty("doNotCancelPopup", DarkPopupMenuUI.HIDE_POPUP_KEY);
        component.putClientProperty("doNotCancelOnScroll", Boolean.TRUE);
    }

    public static boolean isInCell(final Component c) {
        return getParentOfType(CellRendererPane.class, c) != null
                || getParentOfType(TableCellRenderer.class, c) != null
                || getParentOfType(TreeCellRenderer.class, c) != null
                || getParentOfType(CellRenderer.class, c) != null
                || getParentOfType(CellEditor.class, c) != null;
    }

    @SuppressWarnings("unchecked")
    @Nullable
    public static <T> T getParentOfType(final Class<? extends T> cls, final Component c) {
        for (Component eachParent = c; eachParent != null; eachParent = eachParent.getParent()) {
            if (cls.isAssignableFrom(eachParent.getClass())) {
                return (T) eachParent;
            }
        }
        return null;
    }

    @Contract("null -> null")
    @Nullable
    public static Window getWindow(@Nullable final Component component) {
        if (component == null) {
            return null;
        }
        return component instanceof Window ? (Window) component : SwingUtilities.getWindowAncestor(component);
    }

    public static boolean isTooltipShowing(@NotNull final JComponent component) {
        AbstractAction hideTipAction = (AbstractAction) component.getActionMap().get("hideTip");
        return hideTipAction.isEnabled();
    }

    public static MenuElement findEnabledChild(final MenuElement[] e, final int fromIndex, final boolean forward) {
        MenuElement result;
        if (forward) {
            result = nextEnabledChild(e, fromIndex + 1, e.length - 1);
            if (result == null) result = nextEnabledChild(e, 0, fromIndex - 1);
        } else {
            result = previousEnabledChild(e, fromIndex - 1, 0);
            if (result == null) result = previousEnabledChild(e, e.length - 1, fromIndex + 1);
        }
        return result;
    }

    @Nullable
    public static MenuElement findEnabledChild(final MenuElement[] e, final MenuElement elem, final boolean forward) {
        for (int i = 0; i < e.length; i++) {
            if (e[i] == elem) {
                return findEnabledChild(e, i, forward);
            }
        }
        return null;
    }

    @Nullable
    private static MenuElement nextEnabledChild(final MenuElement[] e, final int fromIndex, final int toIndex) {
        for (int i = fromIndex; i <= toIndex; i++) {
            if (e[i] != null) {
                Component comp = e[i].getComponent();
                if (comp != null
                        && (comp.isEnabled() || UIManager.getBoolean("MenuItem.disabledAreNavigable"))
                        && comp.isVisible()) {
                    return e[i];
                }
            }
        }
        return null;
    }

    @Nullable
    private static MenuElement previousEnabledChild(final MenuElement[] e, final int fromIndex, final int toIndex) {
        for (int i = fromIndex; i >= toIndex; i--) {
            if (e[i] != null) {
                Component comp = e[i].getComponent();
                if (comp != null
                        && (comp.isEnabled() || UIManager.getBoolean("MenuItem.disabledAreNavigable"))
                        && comp.isVisible()) {
                    return e[i];
                }
            }
        }
        return null;
    }

    public enum Outline {
        error {
            @Override
            public void setGraphicsColor(final Graphics2D g, final boolean focused) {
                if (focused) {
                    g.setColor(getErrorFocusGlow());
                } else {
                    g.setColor(getErrorGlow());
                }
            }
        },

        warning {
            @Override
            public void setGraphicsColor(final Graphics2D g, final boolean focused) {
                g.setColor(getWarningGlow());
            }
        },

        defaultButton {
            @Override
            public void setGraphicsColor(final Graphics2D g, final boolean focused) {
                if (focused) {
                    g.setColor(getFocusGlow());
                }
            }
        },

        focus {
            @Override
            public void setGraphicsColor(final Graphics2D g, final boolean focused) {
                if (focused) {
                    g.setColor(getFocusGlow());
                }
            }
        };

        public abstract void setGraphicsColor(Graphics2D g, boolean focused);
    }
}
