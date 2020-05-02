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
package com.github.weisj.darklaf.ui.text;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.listener.MouseClickListener;
import com.github.weisj.darklaf.listener.MouseMovementListener;
import com.github.weisj.darklaf.listener.PopupMenuAdapter;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkTextFieldUI extends DarkTextFieldUIBridge implements PropertyChangeListener, MouseClickListener {

    protected static final String KEY_PREFIX = "JTextField.";
    public static final String KEY_VARIANT = KEY_PREFIX + "variant";
    public static final String KEY_SHOW_CLEAR = KEY_PREFIX + "showClear";
    public static final String KEY_FIND_POPUP = KEY_PREFIX + "Search.FindPopup";
    public static final String VARIANT_SEARCH = "search";
    protected static Icon clear;
    protected static Icon clearHover;
    protected static Icon search;
    protected static Icon searchDisabled;
    protected static Icon searchWithHistory;
    protected static Icon searchWithHistoryDisabled;
    protected int arcSize;
    protected int searchArcSize;
    protected int borderSize;
    protected Color background;
    protected Color inactiveBackground;
    private long lastSearchEvent;
    private final PopupMenuListener searchPopupListener = new PopupMenuAdapter() {
        @Override
        public void popupMenuWillBecomeInvisible(final PopupMenuEvent e) {
            lastSearchEvent = System.currentTimeMillis();
        }
    };
    private boolean clearHovered;
    private final MouseMotionListener mouseMotionListener = (MouseMovementListener) e -> updateCursor(e.getPoint());
    private final KeyListener keyListener = new KeyAdapter() {
        @Override
        public void keyTyped(final KeyEvent e) {
            SwingUtilities.invokeLater(() -> {
                Point p = MouseInfo.getPointerInfo().getLocation();
                SwingUtilities.convertPointFromScreen(p, getComponent());
                updateCursor(p);
            });
        }
    };

    public static ComponentUI createUI(final JComponent c) {
        return new DarkTextFieldUI();
    }

    protected static Icon getClearIcon(final boolean clearHovered) {
        return clearHovered ? clearHover : clear;
    }

    protected Rectangle getTextRect(final JTextComponent c) {
        Insets i = c.getInsets();
        Dimension dim = c.getSize();
        Rectangle r = new Rectangle(i.left, i.top, dim.width - i.left - i.right, dim.height - i.top - i.bottom);
        adjustTextRect(c, r);
        return r;
    }

    protected void adjustTextRect(final JTextComponent c, final Rectangle r) {
        int end = r.x + r.width;
        boolean ltr = c.getComponentOrientation().isLeftToRight();
        if (doPaintLeftIcon(c)) {
            Point p = getSearchIconCoord();
            if (ltr) {
                r.x = p.x + getSearchIcon(c).getIconWidth();
            } else {
                end = p.x;
            }
        }
        if (doPaintRightIcon(c)) {
            Point p = getClearIconCoord();
            if (ltr) {
                end = p.x;
            } else {
                r.x = p.x + getClearIcon(false).getIconWidth();
            }
        }
        r.width = end - r.x;
    }

    public static boolean isOver(final Point p, final Icon icon, final Point e) {
        return new Rectangle(p.x, p.y,
                             icon.getIconWidth(), icon.getIconHeight()).contains(e);
    }

    protected void updateCursor(final Point p) {
        ClickAction action = getActionUnder(p);
        boolean oldClear = clearHovered;
        clearHovered = action == ClickAction.RIGHT_ACTION;
        if (oldClear != clearHovered) {
            editor.repaint();
        }
        Rectangle textRect = getTextRect(getComponent());
        if (textRect.contains(p)) {
            getComponent().setCursor(Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR));
        } else {
            Cursor cursor = action == ClickAction.NONE
                    ? Cursor.getDefaultCursor()
                    : Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
            getComponent().setCursor(cursor);
        }
    }

    protected ClickAction getActionUnder(final Point p) {
        JTextComponent c = getComponent();
        if (!c.isEnabled()) return ClickAction.NONE;
        if (c.isEditable()
            && isOver(getClearIconCoord(), getClearIcon(clearHovered), p) && showClearIcon(c)) {
            return ClickAction.RIGHT_ACTION;
        }
        if (isOver(getSearchIconCoord(), getSearchIcon(c), p) && isSearchField(c)) {
            return ClickAction.LEFT_ACTION;
        }
        return ClickAction.NONE;
    }

    private static JPopupMenu getSearchPopup(final JComponent c) {
        return PropertyUtil.getObject(c, KEY_FIND_POPUP, JPopupMenu.class);
    }

    protected Point getSearchIconCoord() {
        Rectangle r = getDrawingRect(getComponent());
        int w = getSearchIcon(getComponent()).getIconWidth();
        return DarkUIUtil.adjustForOrientation(new Point(r.x + borderSize, r.y + (r.height - w) / 2),
                                               w, editor);
    }

    protected static Icon getSearchIcon(final Component c) {
        boolean enabled = c.isEnabled();
        return isSearchFieldWithHistoryPopup(c)
                ? enabled ? searchWithHistory : searchWithHistoryDisabled
                : enabled ? search : searchDisabled;
    }

    public static boolean isSearchFieldWithHistoryPopup(final Component c) {
        return isSearchField(c) && getSearchPopup((JComponent) c) != null;
    }

    public static boolean isSearchField(final Component c) {
        return PropertyUtil.isPropertyEqual(c, KEY_VARIANT, VARIANT_SEARCH);
    }

    public static boolean showClearIcon(final Component c) {
        return PropertyUtil.getBooleanProperty(c, KEY_SHOW_CLEAR);
    }

    @Override
    protected void paintSafely(final Graphics g) {
        GraphicsContext context = new GraphicsContext(g);
        super.paintSafely(g);
        context.restore();
        paintIcons(g);
    }

    protected void paintIcons(final Graphics g) {
        JTextComponent c = getComponent();
        if (doPaintLeftIcon(c)) paintLeftIcon(g);
        if (doPaintRightIcon(c)) paintRightIcon(g);
    }

    protected boolean doPaintLeftIcon(final JTextComponent c) {
        return isSearchField(c);
    }

    protected boolean doPaintRightIcon(final JTextComponent c) {
        return c.getText().length() > 0 && showClearIcon(c);
    }

    protected void paintRightIcon(final Graphics g) {
        paintClearIcon(g);
    }

    protected void paintLeftIcon(final Graphics g) {
        paintSearchIcon(g);
    }

    @Override
    public Rectangle getDrawingRect(final JTextComponent c) {
        int w = borderSize;
        return new Rectangle(w, w, c.getWidth() - 2 * w, c.getHeight() - 2 * w);
    }

    @Override
    protected int getArcSize(final JComponent c) {
        return DarkTextFieldUI.isSearchField(c) ? searchArcSize : arcSize;
    }

    private void paintClearIcon(final Graphics g) {
        Point p = getClearIconCoord();
        getClearIcon(clearHovered || !(editor.isEditable() && editor.isEnabled())).paintIcon(null, g, p.x, p.y);
    }

    private void paintSearchIcon(final Graphics g) {
        Point p = getSearchIconCoord();
        getSearchIcon(editor).paintIcon(null, g, p.x, p.y);
    }

    protected Point getClearIconCoord() {
        Rectangle r = getDrawingRect(getComponent());
        int w = getClearIcon(clearHovered).getIconWidth();
        return DarkUIUtil.adjustForOrientation(new Point(r.x + r.width - w - borderSize, r.y + (r.height - w) / 2),
                                               w, editor);
    }

    protected void showSearchPopup() {
        if (lastSearchEvent == 0 || (System.currentTimeMillis() - lastSearchEvent) > 250) {
            JPopupMenu menu = getSearchPopup(getComponent());
            if (menu != null) {
                menu.show(getComponent(), getSearchIconCoord().x, getComponent().getHeight());
            }
        }
    }

    @Override
    protected DarkCaret.CaretStyle getDefaultCaretStyle() {
        return DarkCaret.CaretStyle.VERTICAL_LINE_STYLE;
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        arcSize = UIManager.getInt("TextField.arc");
        borderSize = UIManager.getInt("TextField.borderThickness");
        searchArcSize = UIManager.getInt("TextField.searchArc");
        background = UIManager.getColor("TextField.background");
        inactiveBackground = UIManager.getColor("TextField.disabledBackground");
        clearHover = UIManager.getIcon("TextField.search.clearHover.icon");
        clear = UIManager.getIcon("TextField.search.clear.icon");
        searchWithHistory = UIManager.getIcon("TextField.search.searchWithHistory.icon");
        searchWithHistoryDisabled = UIManager.getIcon("TextField.search.searchWithHistory.disabled.icon");
        search = UIManager.getIcon("TextField.search.search.icon");
        searchDisabled = UIManager.getIcon("TextField.search.search.disabled.icon");
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        super.propertyChange(evt);
        String key = evt.getPropertyName();
        if (KEY_FIND_POPUP.equals(key)) {
            Object oldVal = evt.getOldValue();
            Object newVal = evt.getNewValue();
            if (oldVal instanceof JPopupMenu) {
                ((JPopupMenu) oldVal).removePopupMenuListener(searchPopupListener);
            }
            if (newVal instanceof JPopupMenu) {
                ((JPopupMenu) newVal).addPopupMenuListener(searchPopupListener);
            }
        } else if (KEY_VARIANT.equals(key)) {
            editor.putClientProperty(KEY_SHOW_CLEAR, isSearchField(editor));
            layoutChanged();
        } else if (KEY_SHOW_CLEAR.equals(key)) {
            layoutChanged();
        }
    }

    protected void layoutChanged() {
        Component parent = editor.getParent();
        if (parent instanceof JComponent) {
            parent.doLayout();
        }
        editor.repaint();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        JTextComponent c = getComponent();
        c.addMouseListener(this);
        c.addMouseMotionListener(mouseMotionListener);
        c.addKeyListener(keyListener);
    }

    @Override
    protected void uninstallListeners() {
        JTextComponent c = getComponent();
        c.removeMouseListener(this);
        c.removeMouseMotionListener(mouseMotionListener);
        c.removeKeyListener(keyListener);
    }

    @Override
    public void mouseClicked(final MouseEvent e) {
        ClickAction actionUnder = getActionUnder(e.getPoint());
        if (actionUnder == ClickAction.RIGHT_ACTION) {
            getComponent().setText("");
        } else if (actionUnder == ClickAction.LEFT_ACTION) {
            showSearchPopup();
        }
    }

    protected enum ClickAction {
        RIGHT_ACTION,
        LEFT_ACTION,
        NONE
    }
}
