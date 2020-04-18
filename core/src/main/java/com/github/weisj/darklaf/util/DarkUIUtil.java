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
package com.github.weisj.darklaf.util;

import com.github.weisj.darklaf.decorators.CellRenderer;
import com.github.weisj.darklaf.ui.popupmenu.DarkPopupMenuUI;
import com.github.weisj.darklaf.ui.table.DarkTableHeaderUI;
import sun.awt.SunToolkit;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.table.TableCellRenderer;
import javax.swing.tree.TreeCellRenderer;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Path2D;
import java.awt.geom.RoundRectangle2D;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public final class DarkUIUtil {

    public static final Color TRANSPARENT_COLOR = new Color(0, 0, 0, 0);
    private static AlphaComposite glowComposite = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f);
    private static AlphaComposite dropComposite = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.8f);
    private static AlphaComposite shadowComposite = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.1f);
    private static Color errorGlow;
    private static Color errorFocusGlow;
    private static Color focusGlow;
    private static Color focusInactiveGlow;
    private static Color warningGlow;
    private static final Rectangle iconRect = new Rectangle();
    private static final Rectangle textRect = new Rectangle();

    public static void setGlowOpacity(final float alpha) {
        glowComposite = glowComposite.derive(alpha);
    }

    public static void setShadowOpacity(final float alpha) {
        shadowComposite = shadowComposite.derive(alpha);
    }

    public static void setDropOpacity(final float alpha) {
        dropComposite = dropComposite.derive(alpha);
    }

    public static AlphaComposite getDropComposite() {
        return dropComposite;
    }

    public static AlphaComposite getShadowComposite() {
        return shadowComposite;
    }

    public static AlphaComposite getGlowComposite() {
        return glowComposite;
    }

    public static void setErrorGlow(final Color errorGlow) {
        DarkUIUtil.errorGlow = errorGlow;
    }

    public static void setErrorFocusGlow(final Color errorFocusGlow) {
        DarkUIUtil.errorFocusGlow = errorFocusGlow;
    }

    public static void setFocusGlow(final Color focusGlow) {
        DarkUIUtil.focusGlow = focusGlow;
    }

    public static void setFocusInactiveGlow(final Color focusInactiveGlow) {
        DarkUIUtil.focusInactiveGlow = focusInactiveGlow;
    }

    public static void setWarningGlow(final Color warningGlow) {
        DarkUIUtil.warningGlow = warningGlow;
    }

    public static Color getErrorGlow() {
        return errorGlow;
    }

    public static Color getErrorFocusGlow() {
        return errorFocusGlow;
    }

    public static Color getFocusGlow() {
        return focusGlow;
    }

    public static Color getFocusInactiveGlow() {
        return focusInactiveGlow;
    }

    public static Color getWarningGlow() {
        return warningGlow;
    }

    private static void doPaint(final Graphics2D g, final float width, final float height, final float arc,
                                final float bw, final boolean inside) {
        GraphicsContext context = GraphicsUtil.setupStrokePainting(g);
        float outerArc = inside ? arc : arc + bw;
        float innerArc = inside ? arc - bw : arc;
        Shape outerRect = new RoundRectangle2D.Float(0, 0, width, height, outerArc, outerArc);
        Shape innerRect = new RoundRectangle2D.Float(bw, bw, width - 2 * bw, height - 2 * bw, innerArc, innerArc);
        Path2D path = new Path2D.Float(Path2D.WIND_EVEN_ODD);
        path.append(outerRect, false);
        path.append(innerRect, false);
        g.fill(path);
        context.restore();
    }

    public static void paintFocusBorder(final Graphics2D g, final int width, final int height, final float arc,
                                        final float bw) {
        paintFocusBorder(g, width, height, arc, bw, true);
    }

    public static void paintFocusBorder(final Graphics2D g, final int width, final int height, final float arc,
                                        final float bw, final boolean active) {
        GraphicsContext config = new GraphicsContext(g);
        g.setComposite(DarkUIUtil.glowComposite);
        paintOutlineBorder(g, width, height, arc, bw, active, Outline.focus);
        config.restore();
    }

    public static void paintOutlineBorder(final Graphics2D g, final int width, final int height, final float arc,
                                          final float bw, final boolean hasFocus, final Outline type) {
        paintOutlineBorder(g, width, height, arc, bw, hasFocus, type, true);
    }

    public static void paintOutlineBorder(final Graphics2D g, final int width, final int height, final float arc,
                                          final float bw, final boolean hasFocus, final Outline type,
                                          final boolean withLineBorder) {
        type.setGraphicsColor(g, hasFocus);
        doPaint(g, width, height, arc, withLineBorder ? bw + getStrokeWidth(g) : bw, false);
    }

    public static void fillFocusRect(final Graphics2D g, final int x, final int y,
                                     final int width, final int height) {
        fillFocusRect(g, x, y, width, height, true);
    }

    public static void fillFocusRect(final Graphics2D g, final int x, final int y,
                                     final int width, final int height, final boolean active) {
        GraphicsContext config = new GraphicsContext(g);
        g.setComposite(DarkUIUtil.glowComposite);
        Outline.focus.setGraphicsColor(g, active);
        g.fillRect(x, y, width, height);
        config.restore();
    }

    public static void paintFocusOval(final Graphics2D g, final int x, final int y,
                                      final int width, final int height) {
        paintFocusOval(g, (float) x, (float) y, (float) width, (float) height);
    }

    public static void paintFocusOval(final Graphics2D g, final float x, final float y,
                                      final float width, final float height) {
        paintFocusOval(g, x, y, width, height, true);
    }

    public static void paintFocusOval(final Graphics2D g, final float x, final float y,
                                      final float width, final float height, final boolean active) {
        GraphicsContext config = new GraphicsContext(g);
        g.setComposite(DarkUIUtil.glowComposite);
        Outline.focus.setGraphicsColor(g, active);

        float blw = 3.0f;
        Path2D shape = new Path2D.Float(Path2D.WIND_EVEN_ODD);
        shape.append(new Ellipse2D.Float(x - blw, y - blw, width + blw * 2, height + blw * 2), false);
        shape.append(new Ellipse2D.Float(x, y, width, height), false);
        g.fill(shape);
        config.restore();
    }

    public static float getStrokeWidth(final Graphics2D g) {
        Stroke stroke = g.getStroke();
        return stroke instanceof BasicStroke ? ((BasicStroke) stroke).getLineWidth() : 1f;
    }

    public static void paintLineBorder(final Graphics2D g, final float x, final float y,
                                       final float width, final float height, final int arc) {
        float lw = getStrokeWidth(g);
        g.translate(x, y);
        doPaint(g, width, height, arc, lw, true);
        g.translate(-x, -y);
    }

    public static void fillRoundRect(final Graphics2D g, final float x, final float y,
                                     final float width, final float height, final int arc) {
        fillRoundRect(g, x, y, width, height, arc, true);
    }

    public static void fillRoundRect(final Graphics2D g, final float x, final float y,
                                     final float width, final float height, final int arc,
                                     final boolean adjustForBorder) {
        float lw = adjustForBorder ? getStrokeWidth(g) / 2f : 0;
        float arcSze = arc - lw;
        g.fill(new RoundRectangle2D.Float(x + lw, y + lw, width - 2 * lw, height - 2 * lw, arcSze, arcSze));
    }


    public static Color blendColors(final Color color1, final Color color2, final double percent) {
        if (percent == 1) return color1;
        if (percent == 0) return color2;
        double inverse_percent = 1.0 - percent;
        int redPart = (int) (color1.getRed() * percent + color2.getRed() * inverse_percent);
        int greenPart = (int) (color1.getGreen() * percent + color2.getGreen() * inverse_percent);
        int bluePart = (int) (color1.getBlue() * percent + color2.getBlue() * inverse_percent);
        return new Color(redPart, greenPart, bluePart);
    }

    public static void applyInsets(final Rectangle rect, final Insets insets) {
        if (insets != null && rect != null) {
            rect.x += insets.left;
            rect.y += insets.top;
            rect.width -= (insets.right + rect.x);
            rect.height -= (insets.bottom + rect.y);
        }
    }

    public static void removeInsets(final Rectangle rectangle, final Insets insets) {
        if (insets != null) {
            rectangle.x += insets.left;
            rectangle.y += insets.top;
            rectangle.width -= insets.left + insets.right;
            rectangle.height -= insets.top + insets.bottom;
        }
    }

    public static void repaint(final JComponent component) {
        if (component != null) component.repaint();
    }

    public static boolean hasFocus(final Component c) {
        return hasFocus(c, null);
    }

    /**
     * Returns whether the component has the focus, or one of the subcomponents has it.
     *
     * @param c the component.
     * @param e an event associated with focusLost. optional (i.e. can be null).
     * @return true if the component or one of its subcomponents has the focus.
     */
    public static boolean hasFocus(final Component c, final FocusEvent e) {
        if (c == null) return false;
        if (c.hasFocus()) return true;
        if (c instanceof Window) {
            return hasFocus(c);
        }
        Component owner = null;
        if (e != null) {
            owner = e.getOppositeComponent();
        }
        if (owner == null) {
            owner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
        }
        return (owner != null && SwingUtilities.isDescendingFrom(owner, c));
    }

    public static boolean hasFocus(final Window w) {
        Component owner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
        if (owner == null) return false;
        return SwingUtilities.getWindowAncestor(owner) == w;
    }

    public static Container getUnwrappedParent(final Container comp) {
        if (comp == null) return null;
        return SwingUtilities.getUnwrappedParent(comp);
    }

    public static Container getUnwrappedParent(final Component comp) {
        if (comp == null) return null;
        return SwingUtilities.getUnwrappedParent(comp);
    }

    public static int getFocusAcceleratorKeyMask() {
        Toolkit tk = Toolkit.getDefaultToolkit();
        if (tk instanceof SunToolkit) {
            return ((SunToolkit) tk).getFocusAcceleratorKeyMask();
        }
        return ActionEvent.ALT_MASK;
    }


    public static Object getUIOfType(final ComponentUI ui, final Class<?> klass) {
        if (klass.isInstance(ui)) {
            return ui;
        }
        return null;
    }

    public static void doNotCancelPopupSetup(final JComponent component) {
        component.putClientProperty(DarkPopupMenuUI.KEY_DO_NOT_CANCEL_POPUP, DarkPopupMenuUI.HIDE_POPUP_VALUE);
        component.putClientProperty(DarkPopupMenuUI.KEY_DO_NOT_CANCEL_ON_SCROLL, Boolean.TRUE);
    }

    public static boolean isInCell(final Component c) {
        boolean tableHeaderCell = c instanceof JComponent && Boolean.TRUE.equals(
            ((JComponent) c).getClientProperty(DarkTableHeaderUI.KEY_IS_HEADER_RENDERER));
        boolean inCellRenderer = !tableHeaderCell
                                 && (getParentOfType(CellRendererPane.class, c) != null
                                     || getParentOfType(TableCellRenderer.class, c) != null
                                     || getParentOfType(TreeCellRenderer.class, c) != null
                                     || getParentOfType(CellRenderer.class, c) != null
                                     || getParentOfType(CellEditor.class, c) != null);
        return inCellRenderer && getParentOfType(JComboBox.class, c) == null;
    }

    @SuppressWarnings("unchecked")

    public static <T> T getParentOfType(final Class<? extends T> cls, final Component c) {
        for (Component eachParent = c; eachParent != null; eachParent = eachParent.getParent()) {
            if (cls.isAssignableFrom(eachParent.getClass())) {
                return (T) eachParent;
            }
        }
        return null;
    }


    public static Window getWindow(final Component component) {
        if (component == null) {
            return null;
        }
        return component instanceof Window ? (Window) component : SwingUtilities.getWindowAncestor(component);
    }

    public static boolean isTooltipShowing(final JComponent component) {
        AbstractAction hideTipAction = (AbstractAction) component.getActionMap().get("hideTip");
        return hideTipAction.isEnabled();
    }


    public static MenuElement findEnabledChild(final MenuElement[] e, final MenuElement elem, final boolean forward) {
        for (int i = 0; i < e.length; i++) {
            if (e[i] == elem) {
                return findEnabledChild(e, i, forward);
            }
        }
        return null;
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

    public static void drawRect(final Graphics g, final Rectangle rect, final int thickness) {
        drawRect(g, rect.x, rect.y, rect.width, rect.height, thickness);
    }

    public static void drawRect(final Graphics g, final int x, final int y,
                                final int width, final int height, final int thickness) {
        g.fillRect(x, y, width, thickness);
        g.fillRect(x, y + thickness, thickness, height - 2 * thickness);
        g.fillRect(x + width - thickness, y + thickness, thickness, height - 2 * thickness);
        g.fillRect(x, y + height - thickness, width, thickness);
    }

    public static boolean isOverText(final MouseEvent e, final int index, final JList<?> list) {
        Rectangle bounds = list.getCellBounds(index, index);
        if (!bounds.contains(e.getPoint())) return false;
        //noinspection unchecked
        Component cellRenderer = ((ListCellRenderer<Object>) list.getCellRenderer())
            .getListCellRendererComponent(list, list.getModel().getElementAt(index),
                                          index, false, false);
        if (cellRenderer instanceof JLabel) {
            return isOverText((JLabel) cellRenderer, bounds, e.getPoint());
        } else {
            return true;
        }
    }

    public static boolean isOverText(final JLabel label, final Rectangle bounds, final Point p) {
        textRect.setBounds(0, 0, 0, 0);
        iconRect.setBounds(0, 0, 0, 0);
        SwingUtilities.layoutCompoundLabel(label, label.getFontMetrics(label.getFont()), label.getText(),
                                           label.getIcon(), label.getVerticalAlignment(),
                                           label.getHorizontalAlignment(),
                                           label.getVerticalTextPosition(), label.getHorizontalTextPosition(),
                                           bounds, iconRect, textRect, label.getIconTextGap());
        return textRect.contains(p);
    }

    public static boolean isOverText(final MouseEvent e, final int row, final int column,
                                     final JTable table) {
        Rectangle bounds = table.getCellRect(row, column, false);
        if (!bounds.contains(e.getPoint())) return false;
        Component cellRenderer = table.getCellRenderer(row, column).getTableCellRendererComponent(
            table, table.getValueAt(row, column), false, false, row, column);
        if (cellRenderer instanceof JLabel) {
            return isOverText((JLabel) cellRenderer, bounds, e.getPoint());
        } else {
            return true;
        }
    }

    public static boolean isMenuShortcutKeyDown(final InputEvent event) {
        return (event.getModifiersEx() & InputEvent.CTRL_DOWN_MASK) != 0;
    }

    @SuppressWarnings("SuspiciousNameCombination")
    public static void rotateRectangle(final Rectangle rect) {
        int tmp = rect.x;
        rect.x = rect.y;
        rect.y = tmp;
        tmp = rect.width;
        rect.width = rect.height;
        rect.height = tmp;
    }

    @SuppressWarnings("SuspiciousNameCombination")
    public static void rotatePoint(final Point p) {
        int tmp = p.x;
        p.x = p.y;
        p.y = tmp;
    }

    public static Insets getBorderInsets(final JComponent comp) {
        Border border = comp.getBorder();
        if (border == null) return new InsetsUIResource(0, 0, 0, 0);
        return border.getBorderInsets(comp);
    }

    public static Point adjustForOrientation(final Point p, final int w, final Component c) {
        if (!c.getComponentOrientation().isLeftToRight()) {
            p.x = c.getWidth() - p.x - w;
        }
        return p;
    }

    public static Rectangle getScreenBounds(final JComponent target, final Point p) {
        return getScreenBounds(target, p.x, p.y);
    }

    public static Rectangle getScreenBounds(final JComponent target, final int x, final int y) {
        return getScreenBounds(target, x, y, true);
    }

    public static Rectangle getScreenBounds(final JComponent target, final int x, final int y,
                                            final boolean subtractInsets) {
        GraphicsConfiguration gc = getDrawingGC(x, y);
        if (gc == null) {
            gc = target.getGraphicsConfiguration();
        }

        Rectangle sBounds = gc.getBounds();
        if (subtractInsets) {
            // Take into account screen insets, decrease viewport
            Insets screenInsets = Toolkit.getDefaultToolkit()
                                         .getScreenInsets(gc);
            sBounds.x += screenInsets.left;
            sBounds.y += screenInsets.top;
            sBounds.width -= (screenInsets.left + screenInsets.right);
            sBounds.height -= (screenInsets.top + screenInsets.bottom);
        }
        return sBounds;
    }

    private static GraphicsConfiguration getDrawingGC(final int x, final int y) {
        GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice[] devices = env.getScreenDevices();
        for (GraphicsDevice device : devices) {
            GraphicsConfiguration config = device.getDefaultConfiguration();
            Rectangle rect = config.getBounds();
            if (rect.contains(x, y)) {
                return config;
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
            public void setGraphicsColor(final Graphics2D g, final boolean active) {
                if (active) {
                    g.setColor(getFocusGlow());
                } else {
                    g.setColor(getFocusInactiveGlow());
                }
            }
        };

        public abstract void setGraphicsColor(Graphics2D g, boolean focused);
    }
}
