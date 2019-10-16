package com.weis.darklaf.ui.text;

import com.weis.darklaf.decorators.MouseClickListener;
import com.weis.darklaf.decorators.MouseMovementListener;
import com.weis.darklaf.decorators.PopupMenuAdapter;
import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.GraphicsContext;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkTextFieldUI extends DarkTextFieldUIBridge implements PropertyChangeListener {

    private final FocusListener focusListener = new FocusAdapter() {
        public void focusGained(final FocusEvent e) {
            getComponent().repaint();
        }

        public void focusLost(final FocusEvent e) {
            getComponent().repaint();
        }
    };
    private long lastSearchEvent;
    private final PopupMenuListener searchPopupListener = new PopupMenuAdapter() {
        @Override
        public void popupMenuWillBecomeInvisible(@NotNull final PopupMenuEvent e) {
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
    protected int arcSize;
    protected int searchArcSize;
    protected static Icon clear;
    protected static Icon clearHover;
    protected static Icon search;
    protected static Icon searchWithHistory;
    protected int borderSize;
    protected Color background;
    protected Color inactiveBackground;

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkTextFieldUI();
    }

    @Contract(pure = true)
    protected static Icon getClearIcon(final boolean clearHovered) {
        return clearHovered ? clearHover : clear;
    }

    @NotNull
    public static Rectangle getTextRect(@NotNull final JComponent c) {
        Insets i = c.getInsets();
        Dimension dim = c.getSize();
        return new Rectangle(i.left, i.top, dim.width - i.left - i.right, dim.height - i.top - i.bottom);
    }

    protected static Icon getSearchIcon(final Component c) {
        return isSearchFieldWithHistoryPopup(c) ? searchWithHistory : search;
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
        search = UIManager.getIcon("TextField.search.search.icon");

    }

    public static boolean chooseAlternativeArc(@NotNull final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JTextField.alternativeArc"));
    }

    public static boolean isOver(@NotNull final Point p, @NotNull final Icon icon, final Point e) {
        return new Rectangle(p.x, p.y,
                             icon.getIconWidth(), icon.getIconHeight()).contains(e);
    }

    protected void updateCursor(final Point p) {
        var action = getActionUnder(p);
        boolean oldClear = clearHovered;
        clearHovered = action == ClickAction.CLEAR;
        if (oldClear != clearHovered) {
            editor.repaint();
        }
        var drawRect = getDrawingRect(getComponent());
        var textRect = getTextRect(getComponent());
        int rightBoundary = getComponent().getText().isEmpty()
                            ? drawRect.x + drawRect.width
                            : getClearIconCoord().x;
        boolean insideTextArea = drawRect.contains(p) && p.x >= textRect.x && p.x < rightBoundary;
        if (insideTextArea) {
            getComponent().setCursor(Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR));
        } else {
            var cursor = action == ClickAction.NONE
                         ? Cursor.getDefaultCursor()
                         : Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
            getComponent().setCursor(cursor);
        }
    }

    protected void showSearchPopup() {
        if (lastSearchEvent == 0 || (System.currentTimeMillis() - lastSearchEvent) > 250) {
            var menu = getSearchPopup(getComponent());
            if (menu != null) {
                menu.show(getComponent(), getSearchIconCoord().x, getComponent().getHeight());
            }
        }
    }

    @Nullable
    private static JPopupMenu getSearchPopup(@NotNull final JComponent c) {
        Object value = c.getClientProperty("JTextField.Search.FindPopup");
        return value instanceof JPopupMenu ? (JPopupMenu) value : null;
    }

    protected Point getSearchIconCoord() {
        Rectangle r = getDrawingRect(getComponent());
        int w = getSearchIcon(getComponent()).getIconWidth();
        return new Point(r.x + DarkTextBorder.PADDING, r.y + (r.height - w) / 2);
    }

    protected void paintBorderBackground(@NotNull final Graphics2D g, @NotNull final JTextComponent c) {
        g.setColor(getBackgroundColor(c));
        Rectangle r = getDrawingRect(getComponent());
        int arc = getArcSize(c);
        DarkUIUtil.paintRoundRect(g, r.x, r.y, r.width, r.height, arc);
    }

    protected Color getBackgroundColor(@NotNull final JTextComponent c) {
        return c.isEnabled() && c.isEditable() ? background : inactiveBackground;
    }

    public static boolean isSearchFieldWithHistoryPopup(final Component c) {
        return isSearchField(c) && getSearchPopup((JComponent) c) != null;
    }

    @Contract("null -> false")
    public static boolean isSearchField(final Component c) {
        return c instanceof JTextField && "search".equals(((JTextField) c).getClientProperty("JTextField.variant"));
    }

    @NotNull
    @Contract("_ -> new")
    public Rectangle getDrawingRect(@NotNull final JTextComponent c) {
        int w = borderSize;
        return new Rectangle(w, w, c.getWidth() - 2 * w, c.getHeight() - 2 * w);
    }

    protected int getArcSize(final Component c) {
        boolean alt = chooseAlternativeArc(c);
        if (!alt && !isSearchField(c)) {
            return 0;
        }
        return DarkTextFieldUI.isSearchField(c) ? (alt ? arcSize : searchArcSize)
                                                : (alt ? searchArcSize : arcSize);
    }

    protected void paintSearchField(@NotNull final Graphics2D g, @NotNull final JTextComponent c) {
        g.setColor(getBackgroundColor(c));
        Rectangle r = getDrawingRect(getComponent());
        int arc = getArcSize(c);
        DarkUIUtil.paintRoundRect(g, r.x, r.y, r.width, r.height, arc);
        paintSearchIcon(g);
        if (c.getText().length() > 0) {
            paintClearIcon(g);
        }
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
        return new Point(r.x + r.width - w - DarkTextBorder.PADDING, r.y + (r.height - w) / 2);
    }

    private ClickAction getActionUnder(final Point p) {
        var c = getComponent();
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

    @Override
    protected DarkCaret.CaretStyle getDefaultCaretStyle() {
        return DarkCaret.CaretStyle.THICK_VERTICAL_LINE_STYLE;
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if ("JTextField.Search.FindPopup".equals(key)) {
            var oldVal = evt.getOldValue();
            var newVal = evt.getNewValue();
            if (oldVal instanceof JPopupMenu) {
                ((JPopupMenu) oldVal).removePopupMenuListener(searchPopupListener);
            }
            if (newVal instanceof JPopupMenu) {
                ((JPopupMenu) newVal).addPopupMenuListener(searchPopupListener);
            }
        }
    }

    @Override
    protected void installListeners() {
        JTextComponent c = getComponent();
        c.addMouseListener(mouseListener);
        c.addMouseMotionListener(mouseMotionListener);
        c.addFocusListener(focusListener);
        c.addKeyListener(keyListener);
    }

    @Override
    protected void uninstallListeners() {
        JTextComponent c = getComponent();
        c.removeMouseListener(mouseListener);
        c.removeMouseMotionListener(mouseMotionListener);
        c.removeFocusListener(focusListener);
        c.removeKeyListener(keyListener);
    }

    protected void paintBackground(final Graphics graphics) {
        Graphics2D g = (Graphics2D) graphics;
        JTextComponent c = this.getComponent();

        Container parent = c.getParent();
        if (c.isOpaque() && parent != null) {
            g.setColor(parent.getBackground());
            g.fillRect(0, 0, c.getWidth(), c.getHeight());
        }

        GraphicsContext config = new GraphicsContext(g);
        Border border = c.getBorder();
        if (isSearchField(c)) {
            paintSearchField(g, c);
        } else if (border instanceof DarkTextBorder) {
            paintBorderBackground(g, c);
        } else {
            super.paintBackground(g);
        }
        config.restore();
    }

    private enum ClickAction {
        CLEAR,
        SEARCH_POPUP,
        NONE
    }
}
