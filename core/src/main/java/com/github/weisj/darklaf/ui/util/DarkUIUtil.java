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
package com.github.weisj.darklaf.ui.util;

import java.awt.*;
import java.awt.event.FocusEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.geom.AffineTransform;
import java.beans.PropertyChangeEvent;
import java.util.Objects;
import java.util.function.Predicate;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.TreeCellRenderer;

import com.github.weisj.darklaf.focus.FocusParentHelper;
import com.github.weisj.darklaf.iconset.IconSet;
import com.github.weisj.darklaf.properties.icons.IconLoader;
import com.github.weisj.darklaf.properties.icons.IconResolver;
import com.github.weisj.darklaf.ui.cell.CellRenderer;
import com.github.weisj.darklaf.ui.popupmenu.DarkPopupMenuUI;
import com.github.weisj.darklaf.ui.popupmenu.MouseGrabber;
import com.github.weisj.darklaf.ui.popupmenu.MouseGrabberUtil;
import com.github.weisj.darklaf.ui.table.header.DarkTableHeaderRendererPane;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public final class DarkUIUtil {

    private static final int CELL_SEARCH_DEPTH = 3;

    public static IconResolver iconResolver() {
        return IconSet.iconLoader();
    }

    /**
     * Only exists for {@link com.github.weisj.darklaf.components.ColoredRadioButton}
     *
     * @return the icon loader housing the radio button icons.
     */
    public static IconLoader radioButtonLoader() {
        return IconLoader.get(IconSet.class);
    }

    private static final Rectangle iconRect = new Rectangle();
    private static final Rectangle textRect = new Rectangle();

    public static Border getUnwrappedBorder(final JComponent c) {
        Border b = c.getBorder();
        while (b instanceof CompoundBorder) {
            b = ((CompoundBorder) b).getOutsideBorder();
        }
        return b;
    }

    public static Rectangle applyInsets(final Rectangle rect, final Insets insets) {
        if (insets != null && rect != null) {
            rect.x += insets.left;
            rect.y += insets.top;
            rect.width -= insets.right + insets.left;
            rect.height -= insets.bottom + insets.top;
        }
        return rect;
    }

    public static Insets invert(final Insets ins) {
        return scale(ins, -1);
    }

    public static Insets scale(final Insets ins, final int factor) {
        if (ins == null) return null;
        ins.left *= factor;
        ins.right *= factor;
        ins.top *= factor;
        ins.bottom *= factor;
        return ins;
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

    public static Dimension addInsets(final Dimension dim, final Insets ins) {
        if (dim == null || ins == null) return dim;
        dim.width += ins.left + ins.right;
        dim.height += ins.top + ins.bottom;
        return dim;
    }

    public static void repaint(final Component component) {
        if (component != null && component.isVisible()) component.repaint();
    }

    public static void repaint(final JComponent component, final Rectangle bounds) {
        repaint(component, bounds.x, bounds.y, bounds.width, bounds.height, false);
    }

    public static void repaint(final JComponent component, final Rectangle bounds, final boolean immediately) {
        repaint(component, bounds.x, bounds.y, bounds.width, bounds.height, immediately);
    }

    public static void repaint(final JComponent component, final int x, final int y, final int width, final int height,
            final boolean immediately) {
        if (component != null && component.isVisible()) {
            if (immediately) {
                component.paintImmediately(x, y, width, height);
            } else {
                component.repaint(x, y, width, height);
            }
        }
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
        Component focusParent = PropertyUtil.getObject(c, FocusParentHelper.KEY_FOCUS_PARENT, Component.class);
        if (c instanceof JComponent) {
            FocusParentHelper.updateFocusParentRegistry((JComponent) c, focusParent);
        }
        return hasFocusImpl(c, focusParent, e);
    }

    public static boolean hasFocusImpl(final Component c, final Component focusParent, final FocusEvent e) {
        if (c == null) return false;
        if (c.hasFocus() || (focusParent != null && focusParent.hasFocus())) return true;
        Component owner = null;
        if (e != null) {
            owner = e.getOppositeComponent();
        }
        if (owner == null) {
            owner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
        }
        JPopupMenu popupMenu = getOpenPopupMenu();
        if (popupMenu != null) {
            owner = popupMenu.getInvoker();
        }
        return owner != null && isDescendingFrom(owner, c, focusParent);
    }

    private static JPopupMenu getOpenPopupMenu() {
        MouseGrabber grabber = MouseGrabberUtil.currentGrabber();
        if (grabber == null) return null;
        MenuElement menuElement = grabber.selectedElement();
        if (menuElement == null) return null;
        return getParentOfType(JPopupMenu.class, menuElement.getComponent(), 2);
    }

    private static boolean isDescendingFrom(final Component a, final Component b1, final Component b2) {
        if (a == b1 || a == b2) return true;
        for (Container p = a.getParent(); p != null; p = p.getParent()) {
            if (p == b1 || p == b2) return true;
        }
        return false;
    }

    public static boolean hasFocus(final Window w) {
        Component owner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
        if (owner == null) return false;
        return SwingUtilities.getWindowAncestor(owner) == w;
    }

    public static Container getUnwrappedParent(final Component comp) {
        if (comp == null) return null;
        return SwingUtilities.getUnwrappedParent(comp);
    }

    public static <T> T getUIOfType(final ComponentUI ui, final Class<T> klass) {
        if (klass.isAssignableFrom(ui.getClass())) {
            return klass.cast(ui);
        }
        return null;
    }

    public static void doNotCancelPopupSetup(final JComponent component) {
        component.putClientProperty(DarkPopupMenuUI.KEY_DO_NOT_CANCEL_POPUP, DarkPopupMenuUI.HIDE_POPUP_VALUE);
        component.putClientProperty(DarkPopupMenuUI.KEY_DO_NOT_CANCEL_ON_SCROLL, Boolean.TRUE);
    }

    public static boolean isInCell(final Component c) {
        if (getParentOfType(DarkTableHeaderRendererPane.class, c, CELL_SEARCH_DEPTH) != null) return false;
        return getParentOfType(c, CELL_SEARCH_DEPTH, CellRendererPane.class, CellEditor.class, TableCellRenderer.class,
                TableCellEditor.class, TreeCellRenderer.class, TreeCellEditor.class, ListCellRenderer.class,
                CellRenderer.class) != null;
    }

    public static <T> T getParentOfType(final Class<? extends T> cls, final Component c) {
        return getParentOfType(cls, c, Integer.MAX_VALUE);
    }

    public static <T> T getParentOfType(final Class<? extends T> cls, final Component c, final int searchDepth) {
        int depth = 0;
        for (Component eachParent = c; eachParent != null; eachParent = eachParent.getParent()) {
            if (cls.isAssignableFrom(eachParent.getClass())) {
                return cls.cast(eachParent);
            }
            if (depth >= searchDepth) break;
            depth++;
        }
        return null;
    }

    @SafeVarargs
    public static <T> T getParentOfType(final Component c, final int searchDepth, final Class<? extends T>... classes) {
        int depth = 0;
        for (Component eachParent = c; eachParent != null; eachParent = eachParent.getParent()) {
            for (Class<? extends T> cls : classes) {
                if (cls.isAssignableFrom(eachParent.getClass())) {
                    return cls.cast(eachParent);
                }
            }
            if (depth >= searchDepth) break;
            depth++;
        }
        return null;
    }

    public static Window getWindow(final Component component) {
        if (component == null) {
            return null;
        }
        return component instanceof Window ? (Window) component : SwingUtilities.getWindowAncestor(component);
    }

    public static boolean isOverText(final JLabel label, final Rectangle bounds, final Point p) {
        textRect.setBounds(0, 0, 0, 0);
        iconRect.setBounds(0, 0, 0, 0);
        SwingUtilities.layoutCompoundLabel(label, label.getFontMetrics(label.getFont()), label.getText(),
                label.getIcon(), label.getVerticalAlignment(), label.getHorizontalAlignment(),
                label.getVerticalTextPosition(), label.getHorizontalTextPosition(), bounds, iconRect, textRect,
                label.getIconTextGap());
        return textRect.contains(p);
    }

    public static boolean isOverText(final MouseEvent e, final int row, final int column, final JTable table) {
        Rectangle bounds = table.getCellRect(row, column, false);
        if (!bounds.contains(e.getPoint())) return false;
        Component cellRenderer = table.getCellRenderer(row, column).getTableCellRendererComponent(table,
                table.getValueAt(row, column), false, false, row, column);
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
        if (p == null) return getScreenBounds(target, target.getLocationOnScreen());
        return getScreenBounds(target, p.x, p.y);
    }

    public static Rectangle getScreenBounds(final JComponent target, final int x, final int y) {
        return getScreenBounds(target, x, y, true);
    }

    public static Rectangle getScreenBounds(final JComponent target, final int x, final int y,
            final boolean subtractInsets) {
        GraphicsConfiguration gc = target != null ? target.getGraphicsConfiguration() : null;
        if (gc == null) {
            gc = getGraphicsConfigurationForLocation(x, y);
        }
        if (gc == null) {
            // Should never happen.
            return new Rectangle();
        }

        Rectangle sBounds = gc.getBounds();
        if (subtractInsets) {
            // Take into account screen insets, decrease viewport
            Insets screenInsets = Toolkit.getDefaultToolkit().getScreenInsets(gc);
            sBounds.x += screenInsets.left;
            sBounds.y += screenInsets.top;
            sBounds.width -= screenInsets.left + screenInsets.right;
            sBounds.height -= screenInsets.top + screenInsets.bottom;
        }
        return sBounds;
    }

    public static GraphicsConfiguration getGraphicsConfigurationForLocation(final int x, final int y) {
        if (GraphicsEnvironment.isHeadless()) return null;
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

    public static Container getParentMatching(final Container parent, final Predicate<Container> test) {
        Container p = parent;
        while (p != null && !test.test(p)) {
            p = p.getParent();
        }
        return p;
    }

    public static Container getParentBeforeMatching(final Container parent, final Predicate<Container> test) {
        Container p = parent;
        Container prev = null;
        while (p != null && !test.test(p)) {
            prev = p;
            p = prev.getParent();
        }
        return prev;
    }

    public static Dimension getPreferredSize(final JComponent component) {
        if (component == null) return new Dimension(0, 0);
        LayoutManager layoutManager = component.getLayout();
        Dimension size = null;
        if (layoutManager != null) {
            size = layoutManager.preferredLayoutSize(component);
        }
        return (size != null) ? size : component.getPreferredSize();
    }

    public static Dimension getMinimumSize(final JComponent component) {
        if (component == null) return new Dimension(0, 0);
        LayoutManager layoutManager = component.getLayout();
        Dimension size = null;
        if (layoutManager != null) {
            size = layoutManager.minimumLayoutSize(component);
        }
        return (size != null) ? size : component.getMinimumSize();
    }

    public static <T> T nullableCast(final Class<T> type, final Object o) {
        if (type != null && type.isInstance(o)) return type.cast(o);
        return null;
    }

    public static boolean isDecorated(final Window window) {
        if (window instanceof Frame) return ((Frame) window).isUndecorated();
        if (window instanceof Dialog) return ((Dialog) window).isUndecorated();
        return false;
    }
}
