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

import com.github.weisj.darklaf.decorators.MouseClickListener;
import com.github.weisj.darklaf.decorators.MouseMovementListener;
import com.github.weisj.darklaf.decorators.PopupMenuAdapter;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.GraphicsContext;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkTextFieldUI extends DarkTextFieldUIBridge implements PropertyChangeListener {

    protected static final String KEY_PREFIX = "JTextField.";
    public static final String KEY_VARIANT = KEY_PREFIX + "variant";
    public static final String KEY_KEEP_SELECTION_ON_FOCUS_LOST = KEY_PREFIX + "keepSelectionOnFocusLost";
    public static final String KEY_FIND_POPUP = KEY_PREFIX + "Search.FindPopup";
    public static final String VARIANT_SEARCH = "search";
    protected static Icon clear;
    protected static Icon clearHover;
    protected static Icon search;
    protected static Icon searchWithHistory;
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
    private final MouseListener mouseListener = (MouseClickListener) e -> {
        ClickAction actionUnder = getActionUnder(e.getPoint());
        if (actionUnder == ClickAction.CLEAR) {
            getComponent().setText("");
        } else if (actionUnder == ClickAction.SEARCH_POPUP) {
            showSearchPopup();
        }
    };

    public static ComponentUI createUI(final JComponent c) {
        return new DarkTextFieldUI();
    }

    protected static Icon getClearIcon(final boolean clearHovered) {
        return clearHovered ? clearHover : clear;
    }

    public static Rectangle getTextRect(final JComponent c) {
        Insets i = c.getInsets();
        Dimension dim = c.getSize();
        return new Rectangle(i.left, i.top, dim.width - i.left - i.right, dim.height - i.top - i.bottom);
    }

    public static boolean isOver(final Point p, final Icon icon, final Point e) {
        return new Rectangle(p.x, p.y,
                             icon.getIconWidth(), icon.getIconHeight()).contains(e);
    }

    protected void updateCursor(final Point p) {
        ClickAction action = getActionUnder(p);
        boolean oldClear = clearHovered;
        clearHovered = action == ClickAction.CLEAR;
        if (oldClear != clearHovered) {
            editor.repaint();
        }
        Rectangle drawRect = getDrawingRect(getComponent());
        Rectangle textRect = getTextRect(getComponent());
        int rightBoundary = getComponent().getText().isEmpty()
                ? drawRect.x + drawRect.width
                : getClearIconCoord().x;
        boolean insideTextArea = drawRect.contains(p) && p.x >= textRect.x && p.x < rightBoundary;
        if (insideTextArea) {
            getComponent().setCursor(Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR));
        } else {
            Cursor cursor = action == ClickAction.NONE
                    ? Cursor.getDefaultCursor()
                    : Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
            getComponent().setCursor(cursor);
        }
    }

    private ClickAction getActionUnder(final Point p) {
        Component c = getComponent();
        if (isSearchField(c)) {
            if (isOver(getClearIconCoord(), getClearIcon(clearHovered), p)) {
                return ClickAction.CLEAR;
            }
            if (isOver(getSearchIconCoord(), getSearchIcon(c), p)) {
                return ClickAction.SEARCH_POPUP;
            }
        }
        return ClickAction.NONE;
    }

    private static JPopupMenu getSearchPopup(final JComponent c) {
        Object value = c.getClientProperty(KEY_FIND_POPUP);
        return value instanceof JPopupMenu ? (JPopupMenu) value : null;
    }

    protected Point getSearchIconCoord() {
        Rectangle r = getDrawingRect(getComponent());
        int w = getSearchIcon(getComponent()).getIconWidth();
        return DarkUIUtil.adjustForOrientation(new Point(r.x + borderSize, r.y + (r.height - w) / 2),
                                               w, editor);
    }

    protected static Icon getSearchIcon(final Component c) {
        return isSearchFieldWithHistoryPopup(c) ? searchWithHistory : search;
    }

    public static boolean isSearchFieldWithHistoryPopup(final Component c) {
        return isSearchField(c) && getSearchPopup((JComponent) c) != null;
    }

    public static boolean isSearchField(final Component c) {
        return c instanceof JTextField && VARIANT_SEARCH.equals(((JTextField) c).getClientProperty(KEY_VARIANT));
    }

    protected void paintBackground(final Graphics graphics) {
        Graphics2D g = (Graphics2D) graphics;
        JTextComponent c = this.getComponent();

        GraphicsContext config = new GraphicsContext(g);
        if (isSearchField(c)) {
            Container parent = c.getParent();
            if (c.isOpaque() && parent != null) {
                g.setColor(parent.getBackground());
                g.fillRect(0, 0, c.getWidth(), c.getHeight());
            }
            paintSearchField(g, c);
        } else {
            super.paintBackground(g);
        }
        config.restore();
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

    private void paintClearIcon(final Graphics2D g) {
        Point p = getClearIconCoord();
        getClearIcon(clearHovered).paintIcon(null, g, p.x, p.y);
    }

    private void paintSearchIcon(final Graphics2D g) {
        Point p = getSearchIconCoord();
        getSearchIcon(getComponent()).paintIcon(null, g, p.x, p.y);
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
        editor.putClientProperty(KEY_KEEP_SELECTION_ON_FOCUS_LOST,
                                 UIManager.getBoolean("TextField.keepSelectionOnFocusLost"));
        arcSize = UIManager.getInt("TextField.arc");
        borderSize = UIManager.getInt("TextField.borderThickness");
        searchArcSize = UIManager.getInt("TextField.searchArc");
        background = UIManager.getColor("TextField.background");
        inactiveBackground = UIManager.getColor("TextField.disabledBackground");
        clearHover = UIManager.getIcon("TextField.search.clearHover.icon");
        clear = UIManager.getIcon("TextField.search.clear.icon");
        searchWithHistory = UIManager.getIcon("TextField.search.searchWithHistory.icon");
        search = UIManager.getIcon("TextField.search.search.icon");
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
            editor.doLayout();
            Component parent = editor.getParent();
            if (parent instanceof JComponent) {
                parent.doLayout();
            }
            editor.repaint();
        }
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        JTextComponent c = getComponent();
        c.addMouseListener(mouseListener);
        c.addMouseMotionListener(mouseMotionListener);
        c.addKeyListener(keyListener);
    }

    @Override
    protected void uninstallListeners() {
        JTextComponent c = getComponent();
        c.removeMouseListener(mouseListener);
        c.removeMouseMotionListener(mouseMotionListener);
        c.removeKeyListener(keyListener);
    }

    protected void paintSearchField(final Graphics2D g, final JTextComponent c) {
        g.setColor(c.getBackground());
        Rectangle r = getDrawingRect(getComponent());
        int arc = getArcSize(c);
        DarkUIUtil.fillRoundRect(g, r.x, r.y, r.width, r.height, arc);
        paintSearchIcon(g);
        if (c.getText().length() > 0) {
            paintClearIcon(g);
        }
    }

    private enum ClickAction {
        CLEAR,
        SEARCH_POPUP,
        NONE
    }
}
