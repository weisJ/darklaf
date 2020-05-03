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
package com.github.weisj.darklaf.util;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.geom.AffineTransform;
import java.beans.PropertyChangeEvent;
import java.util.Objects;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.TreeCellRenderer;

import sun.awt.SunToolkit;

import com.github.weisj.darklaf.icons.IconLoader;
import com.github.weisj.darklaf.ui.cell.CellRenderer;
import com.github.weisj.darklaf.ui.popupmenu.DarkPopupMenuUI;
import com.github.weisj.darklaf.ui.table.header.DarkTableHeaderRendererPane;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public final class DarkUIUtil {

    public static final IconLoader ICON_LOADER = IconLoader.get(IconLoader.class);
    private static final Rectangle iconRect = new Rectangle();
    private static final Rectangle textRect = new Rectangle();

    public static void applyInsets(final Rectangle rect, final Insets insets) {
        if (insets != null && rect != null) {
            rect.x += insets.left;
            rect.y += insets.top;
            rect.width -= (insets.right + insets.left);
            rect.height -= (insets.bottom + insets.top);
        }
    }

    public static Insets addInsets(final Insets ins1, final Insets ins2) {
        if (ins2 == null) return ins1;
        if (ins1 != null) {
            ins1.left += ins2.left;
            ins1.right += ins2.right;
            ins1.top += ins2.top;
            ins1.bottom += ins2.bottom;
            return ins1;
        }
        return null;
    }

    public static void removeInsets(final Rectangle rectangle, final Insets insets) {
        if (insets != null && rectangle != null) {
            rectangle.x -= insets.left;
            rectangle.y -= insets.top;
            rectangle.width += insets.left + insets.right;
            rectangle.height += insets.top + insets.bottom;
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
     * @param  c the component.
     * @param  e an event associated with focusLost. optional (i.e. can be null).
     * @return   true if the component or one of its subcomponents has the focus.
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
        if (getParentOfType(DarkTableHeaderRendererPane.class, c) != null) return false;
        boolean inCellRenderer = getParentOfType(c, CellRendererPane.class, CellEditor.class,
                                                 TableCellRenderer.class, TableCellEditor.class,
                                                 TreeCellRenderer.class, TreeCellEditor.class,
                                                 ListCellRenderer.class, CellRenderer.class) != null;
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

    public static <T> T getParentOfType(final Component c, final Class<? extends T>... classes) {
        for (Component eachParent = c; eachParent != null; eachParent = eachParent.getParent()) {
            for (Class<? extends T> cls : classes) {
                if (cls.isAssignableFrom(eachParent.getClass())) {
                    return (T) eachParent;
                }
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

    public static boolean isOverText(final MouseEvent e, final int index, final JList<?> list) {
        Rectangle bounds = list.getCellBounds(index, index);
        if (!bounds.contains(e.getPoint())) return false;
        // noinspection unchecked
        Component cellRenderer = ((ListCellRenderer<Object>) list.getCellRenderer()).getListCellRendererComponent(list,
                                                                                                                  list.getModel()
                                                                                                                      .getElementAt(index),
                                                                                                                  index,
                                                                                                                  false,
                                                                                                                  false);
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
        Component cellRenderer = table.getCellRenderer(row, column).getTableCellRendererComponent(table,
                                                                                                  table.getValueAt(row,
                                                                                                                   column),
                                                                                                  false, false, row,
                                                                                                  column);
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

    public static int setAltGraphMask(final int modifier) {
        return (modifier | InputEvent.ALT_GRAPH_DOWN_MASK);
    }

    public static boolean isScaleChanged(final PropertyChangeEvent ev) {
        return isScaleChanged(ev.getPropertyName(), ev.getOldValue(), ev.getNewValue());
    }

    public static boolean isScaleChanged(final String name, final Object oldValue, final Object newValue) {
        if (oldValue != newValue && "graphicsConfiguration".equals(name)) {
            GraphicsConfiguration newGC = (GraphicsConfiguration) oldValue;
            GraphicsConfiguration oldGC = (GraphicsConfiguration) newValue;
            AffineTransform newTx = newGC != null ? newGC.getDefaultTransform() : null;
            AffineTransform oldTx = oldGC != null ? oldGC.getDefaultTransform() : null;
            return !Objects.equals(newTx, oldTx);
        } else {
            return false;
        }
    }

    public static boolean isUndecorated(final Window window) {
        if (window instanceof Frame) {
            return ((Frame) window).isUndecorated();
        } else if (window instanceof Dialog) {
            return ((Dialog) window).isUndecorated();
        }
        return false;
    }
}
