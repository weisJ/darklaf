package com.weis.darklaf.ui.tabbedpane;

import com.weis.darklaf.components.ScrollPopupMenu;
import com.weis.darklaf.decorators.PopupMenuAdapter;
import com.weis.darklaf.icons.EmptyIcon;
import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.GraphicsContext;
import com.weis.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ContainerEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.beans.PropertyChangeEvent;
import java.util.TooManyListenersException;
import java.util.function.Function;

public class DarkTabbedPaneUI extends DarkTabbedPaneUIBridge {

    private static final TabbedPaneTransferHandler TRANSFER_HANDLER = new TabbedPaneTransferHandler();
    private final FocusListener focusListener = new FocusListener() {
        @Override
        public void focusGained(final FocusEvent e) {
            repaintTab(tabPane.getSelectedIndex());
        }

        @Override
        public void focusLost(final FocusEvent e) {
            repaintTab(tabPane.getSelectedIndex());
        }
    };
    private final Rectangle tabAreaBounds = new Rectangle(0, 0, 0, 0);
    private final Rectangle dropRect = new Rectangle(0, 0, 0, 0);
    private DarkScrollableTabSupport scrollableTabSupport;
    private DarkTabbedPaneScrollLayout scrollLayout;
    private int currentShiftX = 0;
    private int currentShiftXTmp = 0;
    private int scrollShiftX = 0;
    private int currentShiftY = 0;
    private int currentShiftYTmp = 0;
    private int scrollShiftY = 0;
    private int minVisibleOld = -2;
    private int maxVisibleOld = -2;
    private int minVisible = -1;
    private int maxVisible = -1;
    private boolean dndEnabled;
    private int dropTargetIndex = -1;
    private int dropSourceIndex = -1;
    private boolean sourceEqualsTarget;
    private boolean drawDropRect;


    private DarkScrollHandler scrollHandler;
    private Component leadingComp;
    private Component trailingComp;
    private Component northComp;
    private Component eastComp;
    private Component southComp;
    private Component westComp;

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkTabbedPaneUI();
    }

    protected Color getDragBorderColor() {
        return UIManager.getColor("TabbedPane.dragBorderColor");
    }

    protected Action getNewTabAction() {
        var action = tabPane.getClientProperty("JTabbedPane.newTabAction");
        return action instanceof Action ? (Action) action : null;
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        installDragSupport();
    }

    protected void installDragSupport() {
        tabPane.setTransferHandler(TRANSFER_HANDLER);
        try {
            tabPane.getDropTarget().addDropTargetListener(TRANSFER_HANDLER);
            tabPane.getDropTarget().setActive(dndEnabled);
        } catch (TooManyListenersException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        scrollableTabSupport = null;
    }

    @Override
    protected LayoutManager createLayoutManager() {
        if (tabPane.getTabLayoutPolicy() == JTabbedPane.SCROLL_TAB_LAYOUT) {
            scrollLayout = new DarkTabbedPaneScrollLayout();
            return scrollLayout;
        } else { /* WRAP_TAB_LAYOUT */
            return new DarkTabbedPaneLayout();
        }
    }

    @Override
    protected void installComponents() {
        if (scrollableTabLayoutEnabled()) {
            if (tabScroller == null) {
                tabScroller = new DarkScrollableTabSupport(tabPane.getTabPlacement());
                tabPane.add(tabScroller.viewport);
                scrollableTabSupport = (DarkScrollableTabSupport) tabScroller;
            }
        }
        installTabContainer();
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        var ins = tabPane.getClientProperty("JTabbedPane.tabAreaInsets");
        if (ins instanceof Insets) {
            tabAreaInsets = (Insets) ins;
        }
        ins = tabPane.getClientProperty("JTabbedPane.contentBorderInsets");
        if (ins instanceof Insets) {
            contentBorderInsets = (Insets) ins;
        }
        var lead = tabPane.getClientProperty("JTabbedPane.leadingComponent");
        if (lead instanceof Component) {
            leadingComp = (Component) lead;
            tabPane.add(leadingComp);
        }
        var trail = tabPane.getClientProperty("JTabbedPane.trailingComponent");
        if (trail instanceof Component) {
            trailingComp = (Component) trail;
            tabPane.add(trailingComp);
        }
        dndEnabled = Boolean.TRUE.equals(tabPane.getClientProperty("JTabbedPane.dndEnabled"));
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        if (scrollableTabLayoutEnabled()) {
            tabPane.removeMouseListener(getHandler());
            tabPane.removeMouseMotionListener(getHandler());
            tabScroller.viewport.addMouseMotionListener(getScrollHandler());
            tabScroller.viewport.addMouseListener(getScrollHandler());
        }
    }

    @Override
    protected Handler getHandler() {
        if (handler == null) {
            handler = new DarkHandler();
        }
        return handler;
    }

    @Override
    public void setRolloverTab(final int index) {
        int oldRollover = rolloverTabIndex;
        super.setRolloverTab(index);
        if (oldRollover != getRolloverTab()) {
            repaintTab(oldRollover);
            repaintTab(getRolloverTab());
        }
    }

    public void paint(final Graphics g, final JComponent c) {
        int selectedIndex = tabPane.getSelectedIndex();
        int tabPlacement = tabPane.getTabPlacement();

        ensureCurrentLayout();

        // Paint content border and tab area
        if (tabsOverlapBorder) {
            paintContentBorder(g, tabPlacement, selectedIndex);
        }

        int width = tabPane.getWidth();
        int height = tabPane.getHeight();
        int y = calculateTabAreaHeight(tabPlacement, runCount, maxTabHeight);
        int x = calculateTabAreaWidth(tabPlacement, runCount, maxTabWidth);
        switch (tabPlacement) {
            case TOP:
                paintTabAreaBorder(g, tabPlacement, 0, 0, width, y);
                break;
            case BOTTOM:
                paintTabAreaBorder(g, tabPlacement, 0, height - y, width, y + 1);
                break;
            case LEFT:
                paintTabAreaBorder(g, tabPlacement, 0, 0, x, height);
                break;
            case RIGHT:
                paintTabAreaBorder(g, tabPlacement, width - x, 0, x, height);
                break;
        }

        // If scrollable tabs are enabled, the tab area will be
        // painted by the scrollable tab panel instead.
        if (!scrollableTabLayoutEnabled()) { // WRAP_TAB_LAYOUT
            paintTabArea(g, tabPlacement, selectedIndex);
        }
        if (!tabsOverlapBorder) {
            paintContentBorder(g, tabPlacement, selectedIndex);
        }

        if (!scrollableTabLayoutEnabled() && drawDropRect) {
            paintDrop(g);
        }
    }

    protected void paintTabAreaBorder(@NotNull final Graphics g, final int tabPlacement,
                                      final int x, final int y, final int w, final int h) {
        g.setColor(getTabBorderColor());
        switch (tabPlacement) {
            case TOP:
                g.fillRect(x, y + h - 1, w, 1);
                break;
            case BOTTOM:
                g.fillRect(x, y, w, 1);
                break;
            case LEFT:
                g.fillRect(w - 1, y, 1, h);
                break;
            case RIGHT:
                g.fillRect(x, y, 1, h);
                break;
        }
    }

    protected void paintDrop(@NotNull final Graphics g) {
        g.setColor(getDropColor());
        var context = new GraphicsContext(g);
        if (!scrollableTabLayoutEnabled()) {
            ((Graphics2D) g).setComposite(DarkUIUtil.DROP_ALPHA);
        }
        switch (tabPane.getTabPlacement()) {
            case TOP:
                g.fillRect(dropRect.x, dropRect.y, dropRect.width, dropRect.height - 1);
                break;
            case BOTTOM:
                g.fillRect(dropRect.x, dropRect.y + 1, dropRect.width, dropRect.height - 1);
                break;
            case LEFT:
                g.fillRect(dropRect.x, dropRect.y, dropRect.width - 1, dropRect.height);
                break;
            case RIGHT:
                g.fillRect(dropRect.x + 1, dropRect.y, dropRect.width - 1, dropRect.height);
                break;
        }
        context.restore();
    }

    protected Color getDropColor() {
        if (scrollableTabLayoutEnabled()) {
            return UIManager.getColor("TabbedPane.dropFill");
        } else {
            return getTabBackgroundColor(0, false, true);
        }
    }

    protected Color getTabBackgroundColor(final int tabIndex, final boolean isSelected, final boolean hover) {
        if (isSelected) {
            return hover ? UIManager.getColor("TabbedPane.selectHighlight")
                         : UIManager.getColor("TabbedPane.selected");
        } else {
            return hover ? UIManager.getColor("TabbedPane.highlight")
                         : tabPane.getBackgroundAt(tabIndex);
        }
    }

    @Override
    protected void paintCroppedTabEdge(final Graphics g) {
    }

    @Override
    protected void paintFocusIndicator(final Graphics g, final int tabPlacement, final Rectangle[] rects,
                                       final int tabIndex, final Rectangle iconRect,
                                       final Rectangle textRect, final boolean isSelected) {
        if (isSelected) {
            if (!drawFocusBar()) return;
            g.setColor(getAccentColor(DarkUIUtil.hasFocus(tabPane)));
            var r = rects[tabIndex];
            int focusSize = UIManager.getInt("TabbedPane.focusBarHeight");
            switch (tabPlacement) {
                case LEFT:
                    g.fillRect(r.x + r.width - focusSize - 1, r.y, focusSize, r.height);
                    break;
                case RIGHT:
                    g.fillRect(r.x + 1, r.y, focusSize, r.height);
                    break;
                case BOTTOM:
                    g.fillRect(r.x, r.y + 1, r.width, focusSize);
                    break;
                default:
                    g.fillRect(r.x, r.y + r.height - focusSize, r.width, focusSize);
                    break;
            }
        }
    }

    @Override
    protected void paintTabBorder(@NotNull final Graphics g, final int tabPlacement, final int tabIndex,
                                  final int x, final int y, final int w, final int h,
                                  final boolean isSelected) {
        g.setColor(getTabBorderColor());
        switch (tabPlacement) {
            case TOP:
                g.fillRect(x, y + h - 1, w, 1);
                break;
            case BOTTOM:
                g.fillRect(x, y, w, 1);
                break;
            case LEFT:
                g.fillRect(x + w - 1, y, 1, h);
                break;
            case RIGHT:
                g.fillRect(x, y, 1, h);
                break;
        }
    }

    @Override
    protected void paintTabBackground(@NotNull final Graphics g, final int tabPlacement, final int tabIndex,
                                      final int x,
                                      final int y, final int w, final int h,
                                      final boolean isSelected) {
        g.setColor(getTabBackgroundColor(tabIndex, isSelected, getRolloverTab() == tabIndex));
        var r = rects[tabIndex];
        g.fillRect(r.x, r.y, r.width, r.height);
    }

    @Override
    protected void paintContentBorderTopEdge(final Graphics g, final int tabPlacement, final int selectedIndex,
                                             final int x, final int y, final int w, final int h) {
    }

    @Override
    protected void paintContentBorderLeftEdge(final Graphics g, final int tabPlacement, final int selectedIndex,
                                              final int x, final int y, final int w, final int h) {
    }

    @Override
    protected void paintContentBorderBottomEdge(final Graphics g, final int tabPlacement, final int selectedIndex,
                                                final int x, final int y, final int w, final int h) {
    }

    @Override
    protected void paintContentBorderRightEdge(final Graphics g, final int tabPlacement, final int selectedIndex,
                                               final int x, final int y, final int w, final int h) {
    }

    @Override
    public Rectangle getTabBounds(final JTabbedPane pane, final int i) {
        var rect = super.getTabBounds(pane, i);
        if (scrollableTabLayoutEnabled() && rect != null
                && dropTargetIndex >= 0 && i == dropTargetIndex) {
            int tabPlacement = pane.getTabPlacement();
            if (tabPlacement == TOP || tabPlacement == BOTTOM) {
                if (pane.getComponentOrientation().isLeftToRight()) {
                    rect.x -= dropRect.width;
                    rect.width += dropRect.width;
                } else {
                    rect.width += dropRect.width;
                }
            } else if (tabPlacement == LEFT || tabPlacement == RIGHT) {
                rect.y -= dropRect.height;
                rect.height += dropRect.height;
            }
        }
        return rect;
    }

    @Override
    protected int tabForCoordinate(final JTabbedPane pane, final int x, final int y,
                                   final boolean validateIfNecessary) {
        int tab = super.tabForCoordinate(pane, x, y, validateIfNecessary);
        Point p = new Point(x, y);
        if (scrollableTabLayoutEnabled()) {
            translatePointToTabPanel(x, y, p);
            if (tab == -1 && dropTargetIndex >= 0 && dropRect.contains(p)) {
                return dropTargetIndex;
            }
        }
        return tab;
    }

    protected boolean shouldRotateTabRuns(final int tabPlacement) {
        return Boolean.TRUE.equals(tabPane.getClientProperty("JTabbedPane.rotateTabRuns"));
    }

    @Override
    protected Insets getTabAreaInsets(final int tabPlacement) {
        Insets insets = super.getTabAreaInsets(tabPlacement);
        if (leadingComp != null) {
            var b = leadingComp.getPreferredSize();
            if (isHorizontalTabPlacement()) {
                insets.left += b.width;
            } else {
                insets.top += b.height;
            }
        }
        if (trailingComp != null) {
            var b = trailingComp.getPreferredSize();
            if (isHorizontalTabPlacement()) {
                insets.right += b.width;
            } else {
                insets.bottom += b.height;
            }
        }
        return insets;
    }

    protected Insets getContentBorderInsets(final int tabPlacement) {
        Insets insets = (Insets) super.getContentBorderInsets(tabPlacement).clone();
        if (northComp != null) {
            insets.top += northComp.getPreferredSize().height;
        }
        if (southComp != null) {
            insets.bottom += southComp.getPreferredSize().height;
        }
        if (eastComp != null) {
            insets.right += eastComp.getPreferredSize().width;
        }
        if (westComp != null) {
            insets.left += westComp.getPreferredSize().width;
        }
        return insets;
    }

    protected Color getTabBorderColor() {
        return UIManager.getColor("TabbedPane.tabBorderColor");
    }

    protected boolean drawFocusBar() {
        return !Boolean.FALSE.equals(tabPane.getClientProperty("JTabbedPane.drawFocusBar"));
    }

    protected Color getAccentColor(final boolean focus) {
        return focus ? UIManager.getColor("TabbedPane.accentFocus")
                     : UIManager.getColor("TabbedPane.accent");
    }

    protected DarkScrollHandler getScrollHandler() {
        if (scrollHandler == null) {
            scrollHandler = new DarkScrollHandler();
        }
        return scrollHandler;
    }

    public void clearDropIndicator() {
        drawDropRect = false;
        dropTargetIndex = -1;
        tabPane.doLayout();
        tabPane.repaint();
    }

    public void clearSourceIndicator() {
        dropSourceIndex = -1;
        tabPane.doLayout();
        tabPane.repaint();
    }

    public void setSourceIndicator(final int sourceIndex) {
        this.dropSourceIndex = sourceIndex;
        tabPane.doLayout();
        tabPane.repaint();
    }

    public void setDnDIndicatorRect(final int x, final int y, final int width, final int height, final int targetIndex,
                                    final boolean sourceEqualsTarget) {
        dropRect.setBounds(x, y, width, height);
        if (scrollableTabLayoutEnabled()) {
            var p = scrollableTabSupport.viewport.getLocation();
            dropRect.x -= p.x;
            dropRect.y -= p.y;
        }
        drawDropRect = true;
        this.sourceEqualsTarget = sourceEqualsTarget;
        dropTargetIndex = targetIndex;
        tabPane.doLayout();
        tabPane.repaint();
    }

    public Rectangle getTabAreaBounds() {
        if (scrollableTabLayoutEnabled()) {
            return scrollableTabSupport.viewport.getBounds();
        } else {
            return new Rectangle(tabAreaBounds);
        }
    }

    protected Icon getNewTabIcon() {
        return UIManager.getIcon("TabbedPane.newTab.icon");
    }

    protected Icon getMoreTabsIcon() {
        return UIManager.getIcon("TabbedPane.moreTabs.icon");
    }

    protected void layoutLeadingComponent(final Component comp, final int tabWidth, final int tabHeight,
                                          final Insets insets, final int tx, final int ty, final int tabPlacement) {
        var b = leadingComp.getPreferredSize();
        int h = Math.min(tabHeight, b.height);
        int w = Math.min(tabWidth, b.width);
        int centerY = (tabHeight - h) / 2;
        int centerX = (tabWidth - w) / 2;
        switch (tabPlacement) {
            case LEFT:
                comp.setBounds(insets.left + centerX, ty - b.height, w, b.height);
                break;
            case RIGHT:
                comp.setBounds(tx - tabAreaInsets.left + centerX, ty - b.height,
                               w, b.height);
                break;
            case TOP:
                comp.setBounds(tx - b.width, insets.top + centerY, b.width, h);
                break;
            case BOTTOM:
                comp.setBounds(tx - b.width, ty - tabAreaInsets.bottom + centerY,
                               b.width, h);
                break;
        }
    }

    protected void layoutTrailingComponent(final Component comp, final int tabWidth, final int tabHeight,
                                           final Insets insets, final int tx, final int ty,
                                           final int tw, final int th, final int tabPlacement) {
        var b = trailingComp.getPreferredSize();
        int h = Math.min(tabHeight, b.height);
        int w = Math.min(tabWidth, b.width);
        int centerY = (tabHeight - h) / 2;
        int centerX = (tabWidth - w) / 2;
        switch (tabPlacement) {
            case LEFT:
                comp.setBounds(insets.left + centerX, ty + th, w, b.height);
                break;
            case RIGHT:
                comp.setBounds(tx - tabAreaInsets.left + centerX, ty + th,
                               w, b.height);
                break;
            case TOP:
                comp.setBounds(tx + tw, insets.top + centerY, b.width, h);
                break;
            case BOTTOM:
                comp.setBounds(tx + tw, ty - tabAreaInsets.bottom + centerY,
                               b.width, h);
                break;
        }
    }

    protected MouseEvent convertEvent(@NotNull final MouseEvent e) {
        var p = e.getPoint();
        var pos = scrollableTabSupport.viewport.getLocation();
        p.x += pos.x;
        p.y += pos.y;
        return new MouseEvent(e.getComponent(), e.getID(), e.getWhen(), e.getModifiersEx(),
                              p.x, p.y, e.getClickCount(), e.isPopupTrigger(), e.getButton());
    }

    protected class DarkScrollableTabPanel extends ScrollableTabPanel {
        @Override
        public void paintComponent(final Graphics g) {
            super.paintComponent(g);
            if (drawDropRect) {
                paintDrop(g);
            }
        }

        public void doLayout() {
            if (getComponentCount() > 0) {
                for (int i = 0; i < getComponentCount(); i++) {
                    Component child = getComponent(i);
                    if (child == scrollableTabSupport.newTabButton) {
                        boolean leftToRight = tabPane.getComponentOrientation().isLeftToRight();
                        int tabCount = tabPane.getTabCount();
                        var b = child.getPreferredSize();
                        if (isHorizontalTabPlacement()) {
                            int off = dropTargetIndex == tabCount ? dropRect.width : 0;
                            if (leftToRight) {
                                int x = rects[tabCount - 1].x + rects[tabCount - 1].width + off;
                                child.setBounds(x, 0, b.width, maxTabHeight);
                            } else {
                                int x = rects[tabCount - 1].x - off;
                                child.setBounds(x - b.width, 0, b.width, maxTabHeight);
                            }
                        } else {
                            int off = dropTargetIndex == tabCount ? dropRect.height : 0;
                            int y = rects[tabCount - 1].y + rects[tabCount - 1].height + off;
                            child.setBounds(0, y, maxTabWidth, b.height);
                        }
                    } else {
                        child.setBounds(0, 0, getWidth(), getHeight());
                    }
                }
            }
        }
    }

    protected class DarkTabAreaButton extends JButton implements UIResource {

        @Override
        protected void paintComponent(final Graphics g) {
            super.paintComponent(g);
            paintButton(g);
            paintTabAreaBorder(g, tabPane.getTabPlacement(), 0, 0, getWidth(), getHeight());
        }

        protected void paintButton(final Graphics g) {
        }
    }

    protected class NewTabButton extends JPanel implements UIResource {

        private final JButton button;

        protected NewTabButton() {
            button = new JButton();
            button.setIcon(getNewTabIcon());
            button.putClientProperty("JButton.variant", "shadow");
            button.putClientProperty("JButton.buttonType", "square");
            button.putClientProperty("JButton.thin", Boolean.TRUE);
            button.setRolloverEnabled(true);
            button.addActionListener(e -> {
                var action = getNewTabAction();
                if (action != null) {
                    action.actionPerformed(e);
                }
            });
            button.setOpaque(false);
            add(button);
            setOpaque(false);
            setLayout(null);
        }

        @Override
        public void doLayout() {
            var b = button.getPreferredSize();
            int x = (getWidth() - b.width) / 2;
            int y = (getHeight() - b.height) / 2;
            button.setBounds(x, y, b.width, b.height);
        }

        @Override
        protected void paintComponent(final Graphics g) {
            super.paintComponent(g);
            paintTabAreaBorder(g, tabPane.getTabPlacement(), 0, 0, getWidth() + 1, getHeight());
        }

        @Override
        public Dimension getPreferredSize() {
            return button.getPreferredSize();
        }
    }

    protected class MoreTabsButton extends DarkTabAreaButton {

        private final static String INFINITY = "\u221e";
        private final static int PAD = 2;
        private final Icon icon;

        protected MoreTabsButton() {
            icon = getMoreTabsIcon();
            setIcon(EmptyIcon.create(icon.getIconWidth(), icon.getIconHeight()));
            putClientProperty("JButton.variant", "onlyLabel");
            putClientProperty("JButton.buttonType", "square");
            setFont(getFont().deriveFont(8f));
        }

        protected void paintButton(@NotNull final Graphics g) {
            FontMetrics metrics = g.getFontMetrics();
            String label = getLabelString();
            int labelWidth = metrics.stringWidth(label);
            int x = (getWidth() - (icon.getIconWidth() + labelWidth + PAD)) / 2;
            int y = (getHeight() - icon.getIconHeight()) / 2;


            var config = GraphicsUtil.setupAAPainting(g);
            /*
             * These offsets are due to the nature of the used icon. They are applied to match the baseline of
             * the label.properties text.
             * A different icon might need a different offset.
             */
            int off = 5;
            int tabPlacement = tabPane.getTabPlacement();
            if (tabPlacement == TOP) {
                y += 2;
            } else if (tabPlacement == BOTTOM) {
                y -= 1;
            }

            icon.paintIcon(this, g, x, y);
            config.restore();
            config = GraphicsUtil.setupAntialiasing(g);
            g.drawString(label, x + icon.getIconWidth() + PAD, y + icon.getIconHeight() - off);
            config.restore();
        }

        @NotNull
        private String getLabelString() {
            int invisible = Math.min(minVisible - 1 + tabPane.getTabCount() - maxVisible, tabPane.getTabCount());
            return invisible >= 100 ? INFINITY : String.valueOf(invisible);
        }

        @Override
        public Dimension getPreferredSize() {
            var size = super.getPreferredSize();
            var metrics = getFontMetrics(getFont());
            size.width += metrics.stringWidth(getLabelString()) + PAD;
            return size;
        }
    }

    public class DarkScrollableTabViewport extends ScrollableTabViewport {
        private final Point viewPos = new Point(0, 0);

        @Override
        public Point getViewPosition() {
            return viewPos;
        }
    }

    public class DarkScrollableTabSupport extends ScrollableTabSupport implements MouseWheelListener, ActionListener {

        private static final int SCROLL_REWIND_DELAY = 1200;
        protected final ScrollPopupMenu scrollPopupMenu;
        protected final JButton moreTabsButton;
        protected final JComponent newTabButton;
        private final Timer timer;
        private long lastClickEvent;

        public DarkScrollableTabSupport(final int tabPlacement) {
            super(tabPlacement);
            viewport = new DarkScrollableTabViewport();
            tabPanel = new DarkScrollableTabPanel();

            viewport.setView(tabPanel);
            viewport.addMouseWheelListener(this);

            moreTabsButton = new MoreTabsButton();
            moreTabsButton.setVisible(false);
            moreTabsButton.addActionListener(this);

            newTabButton = new NewTabButton();
            newTabButton.setVisible(Boolean.TRUE.equals(tabPane.getClientProperty("JTabbedPane.showNewTabButton")));

            scrollPopupMenu = new ScrollPopupMenu(UIManager.getInt("TabbedPane.maxPopupHeight"));
            PopupMenuListener popupMenuListener = new PopupMenuAdapter() {
                @Override
                public void popupMenuWillBecomeInvisible(@NotNull final PopupMenuEvent e) {
                    lastClickEvent = System.currentTimeMillis();
                }
            };
            scrollPopupMenu.addPopupMenuListener(popupMenuListener);

            tabPane.add(moreTabsButton);
            timer = new Timer(SCROLL_REWIND_DELAY, e -> endScroll());
            timer.setRepeats(false);
        }

        protected void endScroll() {
            currentShiftX += scrollShiftX;
            currentShiftY += scrollShiftY;
            scrollShiftX = 0;
            scrollShiftY = 0;
            scrollLayout.calculateTabRects(tabPane.getTabPlacement(), tabPane.getTabCount());
            updateRollover();
            viewport.repaint();
        }

        protected void updateRollover() {
            var pos = MouseInfo.getPointerInfo().getLocation();
            SwingUtilities.convertPointFromScreen(pos, tabPane);
            setRolloverTab(pos.x, pos.y);
        }

        @Override
        void createButtons() {
            super.createButtons();
            tabPane.remove(scrollForwardButton);
            tabPane.remove(scrollBackwardButton);
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            if (scrollPopupMenu.isVisible()) {
                scrollPopupMenu.setVisible(false);
            } else {
                if (!tabPane.isEnabled()) return;
                if (lastClickEvent == 0 || (System.currentTimeMillis() - lastClickEvent) > 250) {
                    var pref = scrollPopupMenu.getPreferredSize();
                    boolean leftToRight = tabPane.getComponentOrientation().isLeftToRight();
                    switch (tabPane.getTabPlacement()) {
                        case LEFT:
                            scrollPopupMenu.show(moreTabsButton, moreTabsButton.getWidth(),
                                                 moreTabsButton.getHeight() - pref.height);
                            break;
                        case RIGHT:
                            scrollPopupMenu.show(moreTabsButton, -pref.width,
                                                 moreTabsButton.getHeight() - pref.height);
                            break;
                        case TOP:
                            if (leftToRight) {
                                scrollPopupMenu.show(moreTabsButton, moreTabsButton.getWidth() - pref.width,
                                                     moreTabsButton.getHeight());
                            } else {
                                scrollPopupMenu.show(moreTabsButton, 0, moreTabsButton.getHeight());
                            }
                            break;
                        case BOTTOM:
                            if (leftToRight) {
                                scrollPopupMenu.show(moreTabsButton, moreTabsButton.getWidth() - pref.width,
                                                     -pref.height);
                            } else {
                                scrollPopupMenu.show(moreTabsButton, 0, -pref.height);
                            }
                            break;
                    }
                }
            }
        }

        public void hideMoreTabsButton() {
            moreTabsButton.setVisible(false);
        }

        @Override
        public void mouseWheelMoved(final MouseWheelEvent e) {
            if (!tabPane.isEnabled() || tabPane.getTabCount() == 0) return;
            int tabPosition = tabPane.getTabPlacement();
            int scrollAmount = -1 * e.getUnitsToScroll() * e.getScrollAmount();
            int scrolled;
            if (tabPosition == LEFT || tabPosition == RIGHT) {
                if (e.isShiftDown() || !moreTabsButton.isVisible()) return;
                timer.stop();
                scrolled = scroll(scrollAmount, false);
            } else {
                if (!e.isShiftDown() || !moreTabsButton.isVisible()) return;
                timer.stop();
                scrolled = scroll(scrollAmount, true);
            }
            if (scrolled != 0) {
                showMoreTabsButton();
                updateRollover();
                viewport.repaint();
            }
            timer.start();
        }

        private int scroll(final int amount, final boolean horizontal) {
            Dimension size = tabPane.getSize();
            Insets insets = tabPane.getInsets();
            Insets tabAreaInsets = getTabAreaInsets(tabPane.getTabPlacement());
            int tabCount = tabPane.getTabCount();
            int shift;
            if (horizontal) {
                int rightMargin = size.width - (insets.left + insets.right
                        + tabAreaInsets.right + tabAreaInsets.left);
                if (moreTabsButton.isVisible()) {
                    rightMargin -= moreTabsButton.getWidth();
                }
                int low = rects[0].x;
                int high = rects[tabCount - 1].x + rects[tabCount - 1].width;
                shift = Math.abs(amount);
                if (amount > 0) {
                    shift = Math.min(Math.max(-1 * low, 0), shift);
                } else {
                    shift = Math.min(Math.max(high - rightMargin, 0), shift);
                    shift *= -1;
                }
                scrollLayout.commitShiftX(shift, tabCount);
                scrollShiftX += shift;
            } else {
                int bottomMargin = size.height - (insets.bottom + tabAreaInsets.bottom
                        + insets.top + tabAreaInsets.top);
                if (moreTabsButton.isVisible()) {
                    bottomMargin -= moreTabsButton.getHeight();
                }
                int low = rects[0].y;
                int high = rects[tabCount - 1].y + rects[tabCount - 1].height;
                shift = Math.abs(amount);
                if (amount > 0) {
                    shift = Math.min(Math.max(-1 * low, 0), shift);
                } else {
                    shift = Math.min(Math.max(high - bottomMargin, 0), shift);
                    shift *= -1;
                }
                scrollLayout.commitShiftY(shift, tabCount);
                scrollShiftY += shift;
            }
            return shift;
        }

        public void showMoreTabsButton() {
            moreTabsButton.setVisible(true);
            scrollPopupMenu.removeAll();
            if (maxVisible < 0 || minVisible >= tabPane.getTabCount()) {
                scrollLayout.updateVisibleRange(tabPane.getTabPlacement());
            }
            if (minVisible != tabPane.getTabCount() && maxVisible >= 0) {
                for (int i = 0; i < minVisible; i++) {
                    scrollPopupMenu.add(createMenuItem(i));
                }
            }
            for (int i = maxVisible + 1; i < tabPane.getTabCount(); i++) {
                scrollPopupMenu.add(createMenuItem(i));
            }
            moreTabsButton.repaint();
        }

        @NotNull
        @Contract("_ -> new")
        private JMenuItem createMenuItem(final int i) {
            Icon icon = tabPane.getIconAt(i);
            if (icon != null && !tabPane.getComponentAt(i).isEnabled()) {
                icon = tabPane.getDisabledIconAt(i);
            }
            Component comp = tabPane.getComponentAt(i);
            return new JMenuItem(new AbstractAction(tabPane.getTitleAt(i), icon) {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    if (i >= 0 && i <= tabPane.getTabCount()) {
                        //Use component instead of index as index may have changed in between creation
                        //and invocation of action.
                        tabPane.setSelectedComponent(comp);
                        tabPane.doLayout();
                        comp.requestFocus();
                    }
                }
            });
        }
    }

    public class DarkTabbedPaneScrollLayout extends TabbedPaneScrollLayout {

        @Override
        protected Dimension calculateSize(final boolean minimum) {
            int tabPlacement = tabPane.getTabPlacement();
            Insets insets = tabPane.getInsets();
            Insets contentInsets = getContentBorderInsets(tabPlacement);
            Insets tabAreaInsets = getTabAreaInsets(tabPlacement);

            int height = 0;
            int width = 0;
            int cWidth = 0;
            int cHeight = 0;

            // Determine minimum size required to display largest
            // child in each dimension
            //
            for (int i = 0; i < tabPane.getTabCount(); i++) {
                Component component = tabPane.getComponentAt(i);
                if (component != null) {
                    Dimension size = minimum ? component.getMinimumSize() : component.getPreferredSize();
                    if (size != null) {
                        cHeight = Math.max(size.height, cHeight);
                        cWidth = Math.max(size.width, cWidth);
                    }
                }
            }
            // Add content border insets to minimum size
            width += cWidth;
            height += cHeight;
            int tabExtent;

            // Calculate how much space the tabs will need, based on the
            // minimum size required to display largest child + content border
            //
            if (tabPlacement == LEFT || tabPlacement == RIGHT) {
                int tabHeight = calculateTabHeight(tabPlacement, tabPane.getSelectedIndex(),
                                                   getFontMetrics().getHeight());
                if (scrollableTabSupport.moreTabsButton.isVisible()) {
                    tabHeight += scrollableTabSupport.moreTabsButton.getPreferredSize().height;
                }
                height = Math.max(height, tabHeight);
                tabExtent = preferredTabAreaWidth(tabPlacement,
                                                  height - tabAreaInsets.top - tabAreaInsets.bottom);
                width += tabExtent;
            } else {
                int tabWidth = calculateTabWidth(tabPlacement, tabPane.getSelectedIndex(), getFontMetrics());
                if (scrollableTabSupport.moreTabsButton.isVisible()) {
                    tabWidth += scrollableTabSupport.moreTabsButton.getPreferredSize().width;
                }
                width = Math.max(width, tabWidth);
                tabExtent = preferredTabAreaHeight(tabPlacement, width - tabAreaInsets.left - tabAreaInsets.right);
                height += tabExtent;
            }
            return new Dimension(width + insets.left + insets.right + contentInsets.left + contentInsets.right,
                                 height + insets.bottom + insets.top + contentInsets.top + contentInsets.bottom);

        }

        @Override
        protected int preferredTabAreaHeight(final int tabPlacement, final int width) {
            return calculateMaxTabHeight(tabPlacement);
        }

        @Override
        protected int preferredTabAreaWidth(final int tabPlacement, final int height) {
            return calculateMaxTabWidth(tabPlacement);
        }

        public void layoutContainer(final Container parent) {
            int tabPlacement = tabPane.getTabPlacement();
            Insets insets = tabPane.getInsets();
            Insets tabAreaInsets = getTabAreaInsets(tabPlacement);
            int selectedIndex = tabPane.getSelectedIndex();
            Component visibleComponent = getVisibleComponent();

            calculateLayoutInfo();

            Component selectedComponent = null;
            if (selectedIndex < 0) {
                if (visibleComponent != null) {
                    // The last tab was removed, so remove the component
                    setVisibleComponent(null);
                }
            } else {
                selectedComponent = tabPane.getComponentAt(selectedIndex);
            }

            if (tabPane.getTabCount() == 0) {
                scrollableTabSupport.hideMoreTabsButton();
                return;
            }

            boolean shouldChangeFocus = false;

            // In order to allow programs to use a single component
            // as the display for multiple tabs, we will not change
            // the visible component if the currently selected tab
            // has a null component.  This is a bit dicey, as we don't
            // explicitly state we support this in the spec, but since
            // programs are now depending on this, we're making it work.
            //
            if (selectedComponent != null) {
                if (selectedComponent != visibleComponent &&
                        visibleComponent != null) {

                    var owner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
                    if (owner != null && SwingUtilities.isDescendingFrom(owner, visibleComponent)) {
                        shouldChangeFocus = true;
                    }
                }
                setVisibleComponent(selectedComponent);
            }
            int tx, ty, tw, th; // tab area bounds
            int cx, cy, cw, ch; // content area bounds
            Insets contentInsets = getContentBorderInsets(tabPlacement);
            Rectangle bounds = tabPane.getBounds();
            int numChildren = tabPane.getComponentCount();

            if (numChildren > 0) {
                switch (tabPlacement) {
                    case LEFT:
                        tw = calculateTabAreaWidth(tabPlacement, runCount, maxTabWidth);
                        th = bounds.height - insets.top - insets.bottom - tabAreaInsets.top - tabAreaInsets.bottom;
                        tx = insets.left + tabAreaInsets.left;
                        ty = insets.top + tabAreaInsets.top;
                        cx = insets.left + tw + contentInsets.left + tabAreaInsets.left + tabAreaInsets.right;
                        cy = insets.top + contentInsets.top;
                        cw = bounds.width - insets.left - insets.right - tw - contentInsets.left - contentInsets.right
                                - tabAreaInsets.left - tabAreaInsets.right;
                        ch = bounds.height - insets.top - insets.bottom - contentInsets.top - contentInsets.bottom;
                        tw -= tabAreaInsets.left + tabAreaInsets.right;
                        break;
                    case RIGHT:
                        tw = calculateTabAreaWidth(tabPlacement, runCount, maxTabWidth);
                        th = bounds.height - insets.top - insets.bottom - tabAreaInsets.top - tabAreaInsets.bottom;
                        tx = bounds.width - insets.right - tw + tabAreaInsets.left;
                        ty = insets.top + tabAreaInsets.top;
                        cx = insets.left + contentInsets.left;
                        cy = insets.top + contentInsets.top;
                        cw = bounds.width - insets.left - insets.right - tw - contentInsets.left - contentInsets.right;
                        ch = bounds.height - insets.top - insets.bottom - contentInsets.top - contentInsets.bottom;
                        tw -= tabAreaInsets.left + tabAreaInsets.right;
                        break;
                    case BOTTOM:
                        tw = bounds.width - insets.left - insets.right - tabAreaInsets.left - tabAreaInsets.right;
                        th = calculateTabAreaHeight(tabPlacement, runCount, maxTabHeight);
                        tx = insets.left + tabAreaInsets.left;
                        ty = bounds.height - insets.bottom - th + tabAreaInsets.top;
                        cx = insets.left + contentInsets.left;
                        cy = insets.top + contentInsets.top;
                        cw = bounds.width - insets.left - insets.right - contentInsets.left - contentInsets.right;
                        ch = bounds.height - insets.top - insets.bottom - th - contentInsets.top - contentInsets.bottom;
                        th -= tabAreaInsets.top + tabAreaInsets.bottom;
                        break;
                    default:
                        tw = bounds.width - insets.left - insets.right - tabAreaInsets.left - tabAreaInsets.right;
                        th = calculateTabAreaHeight(tabPlacement, runCount, maxTabHeight);
                        tx = insets.left + tabAreaInsets.left;
                        ty = insets.top + tabAreaInsets.top;
                        cx = insets.left + contentInsets.left;
                        cy = insets.top + th + contentInsets.top;
                        cw = bounds.width - insets.left - insets.right - contentInsets.left - contentInsets.right;
                        ch = bounds.height - insets.top - insets.bottom - th - contentInsets.top - contentInsets.bottom;
                        th -= tabAreaInsets.top + tabAreaInsets.bottom;
                        break;
                }
                JButton moreTabs = scrollableTabSupport.moreTabsButton;
                JComponent newTab = scrollableTabSupport.newTabButton;

                for (int i = 0; i < numChildren; i++) {
                    Component child = tabPane.getComponent(i);

                    if (tabScroller != null && child == tabScroller.viewport) {
                        int vw = tw;
                        int vh = th;
                        Dimension butSize = moreTabs.isVisible()
                                            ? moreTabs.getPreferredSize()
                                            : new Dimension(0, 0);
                        boolean showNewTabButton = newTab.isVisible() && newTab.getParent() == tabPane;
                        Dimension butSize2 = showNewTabButton
                                             ? newTab.getPreferredSize()
                                             : new Dimension(0, 0);
                        boolean leftToRight = tabPane.getComponentOrientation().isLeftToRight();
                        if (tabPlacement == LEFT || tabPlacement == RIGHT) {
                            vh = th - butSize.height - butSize2.height;
                            moreTabs.setBounds(tx, ty + vh + butSize2.height, maxTabWidth, butSize.height);
                            if (showNewTabButton) {
                                newTab.setBounds(tx, ty + vh, maxTabWidth, butSize2.height);
                            }
                        } else {
                            if (leftToRight) {
                                vw = tw - butSize.width - butSize2.width;
                                moreTabs.setBounds(tx + vw + butSize2.width, ty, butSize.width, maxTabHeight);
                                if (showNewTabButton) {
                                    newTab.setBounds(tx + vw, ty, butSize2.width, maxTabHeight);
                                }
                            } else {
                                vw = tw - butSize.width - butSize2.width;
                                moreTabs.setBounds(tx, ty, butSize.width, maxTabHeight);
                                if (showNewTabButton) {
                                    newTab.setBounds(tx + butSize.width, ty, butSize2.width, maxTabHeight);
                                }
                                tx += butSize.width + butSize2.width;
                            }
                        }
                        child.setBounds(tx, ty, vw, vh);
                    } else {
                        int tabHeight = maxTabHeight + tabAreaInsets.top + tabAreaInsets.bottom;
                        int tabWidth = maxTabWidth + tabAreaInsets.left + tabAreaInsets.right;
                        int compHeight = ch;
                        int compY = cy;
                        if (northComp != null) {
                            int nh = northComp.getPreferredSize().height;
                            compY -= nh;
                            compHeight += nh;
                        }
                        if (southComp != null) {
                            compHeight += southComp.getPreferredSize().height;
                        }
                        if (child == leadingComp && leadingComp != null) {
                            layoutLeadingComponent(child, tabWidth, tabHeight, insets, tx, ty, tabPlacement);
                        } else if (child == trailingComp && trailingComp != null) {
                            layoutTrailingComponent(child, tabWidth, tabHeight, insets, tx, ty, tw, th, tabPlacement);
                        } else if (child == northComp && northComp != null) {
                            northComp.setBounds(cx, cy - northComp.getPreferredSize().height,
                                                cw, northComp.getPreferredSize().height);
                        } else if (child == southComp && southComp != null) {
                            southComp.setBounds(cx, cy + ch, cw, southComp.getPreferredSize().height);
                        } else if (child == eastComp && eastComp != null) {
                            eastComp.setBounds(cx + cw, compY, eastComp.getPreferredSize().width, compHeight);
                        } else if (child == westComp && westComp != null) {
                            westComp.setBounds(cx - westComp.getPreferredSize().width, compY,
                                               westComp.getPreferredSize().width, compHeight);
                        } else if (child != moreTabs && child != newTab) {
                            child.setBounds(cx, cy, cw, ch);
                        }
                    }
                }
                super.layoutTabComponents();
                if (shouldChangeFocus) {
                    if (!requestFocusForVisibleComponent()) {
                        tabPane.requestFocus();
                    }
                }
            }
        }

        @SuppressWarnings("SuspiciousNameCombination")
        @Override
        protected void calculateTabRects(final int tabPlacement, final int tabCount) {
            FontMetrics metrics = getFontMetrics();
            Dimension size = tabPane.getSize();
            Insets insets = tabPane.getInsets();
            Insets tabAreaInsets = getTabAreaInsets(tabPlacement);
            boolean verticalTabRuns = !isHorizontalTabPlacement();
            boolean leftToRight = tabPane.getComponentOrientation().isLeftToRight();

            if (tabPlacement == LEFT || tabPlacement == RIGHT) {
                maxTabWidth = calculateMaxTabWidth(tabPlacement);
            } else {
                maxTabHeight = calculateMaxTabHeight(tabPlacement);
            }

            runCount = 0;
            selectedRun = -1;

            if (tabCount == 0) return;

            selectedRun = 0;
            runCount = 1;

            Rectangle tabBounds = new Rectangle(0, 0, 0, 0);
            for (int i = 0; i < tabCount; i++) {
                calculateRect(i, tabBounds, metrics, verticalTabRuns, tabPlacement);
            }

            JButton tabsButton = scrollableTabSupport.moreTabsButton;
            Rectangle selectedBounds = new Rectangle(rects[tabPane.getSelectedIndex()]);
            if (!verticalTabRuns) {
                int rightMargin = size.width - (insets.right + tabAreaInsets.right
                        + insets.left + tabAreaInsets.left);
                var p = getMargins(tabPlacement);
                int leftMargin = p.x;
                int returnAt = p.y;

                currentShiftXTmp = currentShiftX;
                shiftTabsX(0, leftMargin, returnAt, tabCount, false);
                if (!(minVisible == maxVisible && minVisible == tabPane.getSelectedIndex())) {
                    selectedBounds.x += currentShiftXTmp;
                    shiftBoundsToVisibleX(selectedBounds, leftMargin, returnAt, tabCount);
                }
                restoreHiddenTabsX(leftMargin, returnAt, tabCount);
                if (tabsButton.isVisible() && tabPane.getSelectedIndex() < maxVisible) {
                    //Shift again. Hiding the the tab button might reveal the last tab.
                    //Only do this if the last visible tab is not currently selected.
                    //Otherwise the selected tab forces the whole tab area the jump by the width of the tab button.
                    int margin = returnAt + tabsButton.getPreferredSize().width;
                    shiftTabsX(0, leftMargin, margin, tabCount, false);
                    if (minVisible > 0 || maxVisible < tabCount - 1) {
                        //Tab button is still visible but may hide a further tab. restore visible bounds.
                        shiftTabsX(0, leftMargin, returnAt, tabCount, false);
                    }
                }
                adjustForDropX(leftMargin, returnAt, tabCount);

                layoutMoreTabsButton(tabCount);
                commitShiftX(currentShiftXTmp, tabCount);
                currentShiftX = currentShiftXTmp;

                if (!leftToRight) {
                    if (tabsButton.isVisible()) {
                        rightMargin -= tabsButton.getWidth();
                    }
                    var newTabButton = scrollableTabSupport.newTabButton;
                    if (newTabButton.isVisible() && newTabButton.getParent() == tabPane) {
                        rightMargin -= newTabButton.getWidth();
                    }
                    for (int i = 0; i < tabCount; i++) {
                        rects[i].x = rightMargin - rects[i].x - rects[i].width;
                    }
                }
                if (scrollableTabSupport.newTabButton.isVisible()) {
                    layoutNewTabButton(true, leftToRight, leftMargin, returnAt, tabCount);
                }

            } else {
                int bottomMargin = size.height - (insets.bottom + tabAreaInsets.bottom
                        + insets.top + tabAreaInsets.top);
                var p = getMargins(tabPlacement);
                int topMargin = p.x;
                int returnAt = p.y;
                currentShiftYTmp = currentShiftY;
                shiftTabsY(0, topMargin, returnAt, tabCount, false);
                if (!(minVisible == maxVisible && minVisible == tabPane.getSelectedIndex())) {
                    selectedBounds.y += currentShiftYTmp;
                    shiftBoundsToVisibleY(selectedBounds, topMargin, returnAt, tabCount);
                }
                restoreHiddenTabsY(topMargin, returnAt, tabCount);
                if (tabsButton.isVisible() && tabPane.getSelectedIndex() < maxVisible) {
                    shiftTabsY(0, topMargin, bottomMargin, tabCount, false);
                    if (minVisible > 0 || maxVisible < tabCount - 1) {
                        shiftTabsY(0, topMargin, returnAt, tabCount, false);
                    }
                }
                adjustForDropY(topMargin, returnAt, tabCount);

                layoutMoreTabsButton(tabCount);
                commitShiftY(currentShiftYTmp, tabCount);
                currentShiftY = currentShiftYTmp;

                if (scrollableTabSupport.newTabButton.isVisible()) {
                    layoutNewTabButton(false, leftToRight, topMargin, returnAt, tabCount);
                }
            }
            tabScroller.tabPanel.setPreferredSize(tabBounds.getSize());
            tabScroller.tabPanel.invalidate();
        }

        private void layoutNewTabButton(final boolean horizontal, final boolean leftToRight,
                                        final int minVal, final int maxVal, final int tabCount) {
            JComponent button = scrollableTabSupport.newTabButton;
            var buttonBounds = button.getPreferredSize();
            if (horizontal) {
                if (leftToRight) {
                    if (rects[tabCount - 1].x + rects[tabCount - 1].width + buttonBounds.width > maxVal) {
                        tabPane.add(button);
                    } else {
                        scrollableTabSupport.tabPanel.add(button);
                    }
                } else {
                    int x = rects[tabCount - 1].x;
                    if (x - buttonBounds.width < minVal) {
                        tabPane.add(button);
                    } else {
                        scrollableTabSupport.tabPanel.add(button);
                    }
                }
            } else {
                if (rects[tabCount - 1].y + rects[tabCount - 1].height + buttonBounds.height > maxVal) {
                    tabPane.add(button);
                } else {
                    scrollableTabSupport.tabPanel.add(button);
                }
            }
        }

        @SuppressWarnings("SuspiciousNameCombination")
        @NotNull
        @Contract("_ -> new")
        private Point getMargins(final int tabPlacement) {
            Dimension size = tabPane.getSize();
            Insets insets = tabPane.getInsets();
            Insets tabAreaInsets = getTabAreaInsets(tabPlacement);
            var tabsButton = scrollableTabSupport.moreTabsButton;
            var newTabsButton = scrollableTabSupport.newTabButton;
            if (isHorizontalTabPlacement()) {
                int leftMargin = 0;
                int returnAt = size.width - (insets.right + tabAreaInsets.right
                        + insets.left + tabAreaInsets.left);
                if (tabsButton.isVisible()) {
                    returnAt -= tabsButton.getPreferredSize().width;
                }
                if (newTabsButton.isVisible() && newTabsButton.getParent() == tabPane) {
                    returnAt -= newTabsButton.getPreferredSize().width;
                }
                return new Point(leftMargin, returnAt);
            } else {
                int topMargin = 0;
                int returnAt = size.height - (insets.bottom + tabAreaInsets.bottom
                        + insets.top + tabAreaInsets.top);
                if (tabsButton.isVisible()) {
                    returnAt -= tabsButton.getPreferredSize().height;
                }
                if (newTabsButton.isVisible() && newTabsButton.getParent() == tabPane) {
                    returnAt -= newTabsButton.getPreferredSize().height;
                }
                return new Point(topMargin, returnAt);
            }
        }

        @SuppressWarnings("SuspiciousNameCombination")
        public void updateVisibleRange(final int tapPlacement) {
            var p = getMargins(tapPlacement);
            if (isHorizontalTabPlacement()) {
                shiftTabsX(0, p.x, p.y, tabPane.getTabCount(), false);
            } else {
                shiftTabsY(0, p.x, p.y, tabPane.getTabCount(), false);
            }
        }

        private void commitShiftX(final int shift, final int tabCount) {
            commitShiftX(0, tabCount - 1, shift, tabCount);
        }

        private void commitShiftX(final int low, final int high, final int shift, final int tabCount) {
            for (int i = Math.max(low, 0); i <= Math.min(high, tabCount - 1); i++) {
                rects[i].x += shift;
            }
        }

        private void commitShiftY(final int shift, final int tabCount) {
            commitShiftY(0, tabCount - 1, shift, tabCount);
        }

        private void commitShiftY(final int low, final int high, final int shift, final int tabCount) {
            for (int i = Math.max(low, 0); i <= Math.min(high, tabCount - 1); i++) {
                rects[i].y += shift;
            }
        }

        private void shiftBoundsToVisibleX(@NotNull final Rectangle selectedBounds, final int leftMargin,
                                           final int rightMargin, final int tabCount) {
            if (selectedBounds.x + selectedBounds.width > rightMargin) {
                //SelectedTab is not fully visible. Covered on right side.
                shiftTabsX(rightMargin - selectedBounds.x - selectedBounds.width,
                           leftMargin, rightMargin, tabCount, true);
            } else if (selectedBounds.x < leftMargin) {
                //SelectedTab is not fully visible. Covered on left side.
                shiftTabsX(-selectedBounds.x + leftMargin, leftMargin, rightMargin, tabCount, true);
            }
        }

        private void shiftBoundsToVisibleY(@NotNull final Rectangle selectedBounds, final int topMargin,
                                           final int bottomMargin, final int tabCount) {
            if (selectedBounds.y + selectedBounds.height > bottomMargin) {
                //SelectedTab is not fully visible. Covered on right side.
                shiftTabsY(bottomMargin - selectedBounds.y - selectedBounds.height,
                           topMargin, bottomMargin, tabCount, true);
            } else if (selectedBounds.y < topMargin) {
                //SelectedTab is not fully visible. Covered on left side.
                shiftTabsY(-selectedBounds.y + topMargin,
                           topMargin, bottomMargin, tabCount, true);
            }
        }

        private void calculateRect(final int i, final Rectangle tabBounds, final FontMetrics metrics,
                                   final boolean verticalTabRuns, final int tabPlacement) {
            Rectangle rect = rects[i];
            if (!verticalTabRuns) {
                if (i > 0) {
                    rect.x = rects[i - 1].x + rects[i - 1].width;
                } else {
                    tabRuns[0] = 0;
                    maxTabWidth = 0;
                    tabBounds.height = maxTabHeight;
                    rect.x = tabBounds.x;
                }
                rect.width = calculateTabWidth(tabPlacement, i, metrics);
                tabBounds.width = rect.x + rect.width;
                maxTabWidth = Math.max(maxTabWidth, rect.width);
                rect.y = tabBounds.y;
                rect.height = maxTabHeight;
            } else {
                if (i > 0) {
                    rect.y = rects[i - 1].y + rects[i - 1].height;
                } else {
                    tabRuns[0] = 0;
                    maxTabHeight = 0;
                    tabBounds.width = maxTabWidth;
                    rect.y = tabBounds.y;
                }
                rect.height = calculateTabHeight(tabPlacement, i, metrics.getHeight());
                tabBounds.height = rect.y + rect.height;
                maxTabHeight = Math.max(maxTabHeight, rect.height);
                rect.x = tabBounds.x;
                rect.width = maxTabWidth;
            }
        }

        private void restoreHiddenTabsX(final int minX, final int maxX, final int tabCount) {
            if (maxVisible < 0 || maxVisible >= tabCount) return;
            int space = Math.max(maxX - rects[maxVisible].x - rects[maxVisible].width - currentShiftXTmp, 0);
            int shift = Math.min(minX - rects[0].x - currentShiftXTmp, space);
            shiftTabsX(shift, minX, maxX, tabCount, true);
        }

        private void restoreHiddenTabsY(final int minY, final int maxY, final int tabCount) {
            if (maxVisible < 0 || maxVisible >= tabCount) return;
            int space = Math.max(maxY - rects[maxVisible].y - rects[maxVisible].height - currentShiftYTmp, 0);
            int shift = Math.min(minY - rects[0].y - currentShiftYTmp, space);
            shiftTabsY(shift, minY, maxY, tabCount, true);
        }

        private void adjustForDropX(final int minX, final int maxX, final int tabCount) {
            if (dropSourceIndex >= 0) {
                //Hide the source tab.
                int shift = rects[dropSourceIndex].width;
                rects[dropSourceIndex].setSize(0, 0);
                commitShiftX(dropSourceIndex + 1, tabCount - 1, -1 * shift, tabCount);
            }
            if (dropTargetIndex >= 0) {
                commitShiftX(dropTargetIndex, tabCount - 1, dropRect.width, tabCount);
            }
            shiftTabsX(0, minX, maxX, tabCount, false);
        }

        private void adjustForDropY(final int minY, final int maxY, final int tabCount) {
            if (dropSourceIndex >= 0) {
                //Hide the source tab.
                int shift = rects[dropSourceIndex].height;
                rects[dropSourceIndex].setSize(0, 0);
                commitShiftY(dropSourceIndex + 1, tabCount - 1, -1 * shift, tabCount);
            }
            if (sourceEqualsTarget && dropTargetIndex >= 0) {
                commitShiftY(dropTargetIndex, tabCount - 1, dropRect.height, tabCount);
            }
            shiftTabsY(0, minY, maxY, tabCount, false);
        }

        private void layoutMoreTabsButton(final int tabCount) {
            final JButton button = scrollableTabSupport.moreTabsButton;
            if (minVisible > 0 || maxVisible < tabCount - 1) {
                if (scrollableTabSupport.moreTabsButton.isVisible()) {
                    if (minVisible != minVisibleOld || maxVisible != maxVisibleOld) {
                        scrollableTabSupport.showMoreTabsButton();
                    }
                } else {
                    scrollableTabSupport.showMoreTabsButton();
                }
                // Update old values.
                minVisibleOld = minVisible;
                maxVisibleOld = maxVisible;
            } else if (button.isVisible()) {
                scrollableTabSupport.hideMoreTabsButton();
            }
        }

        private void shiftTabsX(final int shift, final int minX, final int returnAt,
                                final int tabCount, final boolean updateShift) {
            shiftTabs(shift, minX, returnAt, tabCount, updateShift, true);
        }

        private void shiftTabsY(final int shift, final int minY, final int returnAt,
                                final int tabCount, final boolean updateShift) {
            shiftTabs(shift, minY, returnAt, tabCount, updateShift, false);
        }

        private void shiftTabs(final int shift, final int minVal, final int returnAt,
                               final int tabCount, final boolean updateShift, final boolean isX) {
            int min = -1;
            int max = -1;
            int minStart = minVisible < 0 || minVisible >= tabCount ? 0 : minVisible;
            int maxStart = maxVisible < 0 || maxVisible >= tabCount ? tabCount - 1 : maxVisible;
            int currShift = isX ? currentShiftXTmp + shift : currentShiftYTmp + shift;
            Function<Integer, Boolean> isVisible = isX ? (i -> isVisibleX(i, currShift, minVal, returnAt))
                                                       : (i -> isVisibleY(i, currShift, minVal, returnAt));
            if (isVisible.apply(minStart)) {
                //Descent to find minimum.
                min = minStart;
                for (int i = minStart - 1; i >= 0; i--) {
                    if (isVisible.apply(i)) {
                        min = i;
                    } else {
                        break;
                    }
                }
            } else {
                //Ascent to find minimum.
                for (int i = minStart + 1; i < tabCount; i++) {
                    if (isVisible.apply(i)) {
                        min = i;
                        break;
                    }
                }
            }
            if (min == -1) {
                min = tabCount;
            }
            if (isVisible.apply(maxStart)) {
                //Ascent to find maximum.
                max = maxStart;
                for (int i = maxStart + 1; i < tabCount; i++) {
                    if (isVisible.apply(i)) {
                        max = i;
                    } else {
                        break;
                    }
                }
            } else {
                //Descent to find maximum.
                for (int i = maxStart - 1; i >= 0; i--) {
                    if (isVisible.apply(i)) {
                        max = i;
                        break;
                    }
                }
            }
            minVisible = min;
            maxVisible = max;

            if (updateShift) {
                if (isX) {
                    currentShiftXTmp += shift;
                } else {
                    currentShiftYTmp += shift;
                }
            }
        }

        @Contract(pure = true)
        private boolean isVisibleX(final int i, final int shift, final int minX, final int maxX) {
            int begin = rects[i].x + shift;
            int end = begin + rects[i].width;
            return !(begin >= maxX || end < minX);
        }

        @Contract(pure = true)
        private boolean isVisibleY(final int i, final int shift, final int minX, final int maxX) {
            int begin = rects[i].y + shift;
            int end = begin + rects[i].height;
            return !(begin >= maxX || end < minX);
        }
    }

    public class DarkTabbedPaneLayout extends TabbedPaneLayout {

        /*
         * Non scroll-layout
         */
        @Override
        public void layoutContainer(final Container parent) {
            int tabPlacement = tabPane.getTabPlacement();
            Insets insets = tabPane.getInsets();
            Insets tabAreaInsets = getTabAreaInsets(tabPlacement);
            int selectedIndex = tabPane.getSelectedIndex();
            Component visibleComponent = getVisibleComponent();

            calculateLayoutInfo();

            Component selectedComponent = null;
            if (selectedIndex < 0) {
                if (visibleComponent != null) {
                    // The last tab was removed, so remove the component
                    setVisibleComponent(null);
                }
            } else {
                selectedComponent = tabPane.getComponentAt(selectedIndex);
            }
            int cx, cy, cw, ch;
            int tx, ty, tw, th;
            Insets contentInsets = getContentBorderInsets(tabPlacement);

            boolean shouldChangeFocus = false;

            // In order to allow programs to use a single component
            // as the display for multiple tabs, we will not change
            // the visible component if the currently selected tab
            // has a null component.  This is a bit dicey, as we don't
            // explicitly state we support this in the spec, but since
            // programs are now depending on this, we're making it work.
            //
            if (selectedComponent != null) {
                if (selectedComponent != visibleComponent &&
                        visibleComponent != null) {
                    var owner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
                    if (owner != null && SwingUtilities.isDescendingFrom(owner, visibleComponent)) {
                        shouldChangeFocus = true;
                    }
                }
                setVisibleComponent(selectedComponent);
            }

            Rectangle bounds = tabPane.getBounds();
            int numChildren = tabPane.getComponentCount();

            if (numChildren > 0) {
                switch (tabPlacement) {
                    case LEFT:
                        tw = calculateTabAreaWidth(tabPlacement, runCount, maxTabWidth);
                        tx = insets.left + tabAreaInsets.left;
                        ty = insets.top + tabAreaInsets.top;
                        th = bounds.height - insets.top - tabAreaInsets.top - insets.bottom - tabAreaInsets.bottom;
                        cx = insets.left + tw + contentInsets.left + tabAreaInsets.left + tabAreaInsets.right;
                        cy = insets.top + contentInsets.top;
                        cw = bounds.width - insets.left - insets.right - contentInsets.left - contentInsets.right - tw
                                - tabAreaInsets.left - tabAreaInsets.right;
                        ch = bounds.height - insets.top - insets.bottom - contentInsets.top - contentInsets.bottom;
                        break;
                    case RIGHT:
                        tw = calculateTabAreaWidth(tabPlacement, runCount, maxTabWidth);
                        tx = bounds.width - insets.left - tw - tabAreaInsets.right - tabAreaInsets.left;
                        ty = insets.top + tabAreaInsets.top;
                        th = bounds.height - insets.top - tabAreaInsets.top - insets.bottom - tabAreaInsets.bottom;
                        cx = insets.left + contentInsets.left;
                        cy = insets.top + contentInsets.top;
                        cw = bounds.width - insets.left - insets.right - contentInsets.left - contentInsets.right - tw
                                - tabAreaInsets.left - tabAreaInsets.right;
                        ch = bounds.height - insets.top - insets.bottom - contentInsets.top - contentInsets.bottom;
                        break;
                    case BOTTOM:
                        th = calculateTabAreaHeight(tabPlacement, runCount, maxTabHeight);
                        ty = bounds.height - insets.bottom - th;
                        tx = insets.left + tabAreaInsets.left;
                        tw = bounds.width - insets.left - insets.right - tabAreaInsets.left - tabAreaInsets.right;
                        cx = insets.left + contentInsets.left;
                        cy = insets.top + contentInsets.top;
                        cw = bounds.width - insets.left - insets.right - contentInsets.left - contentInsets.right;
                        ch = bounds.height - th - insets.top - insets.bottom - contentInsets.top - contentInsets.bottom;
                        break;
                    default:
                        ty = insets.top + tabAreaInsets.top;
                        tx = insets.left + tabAreaInsets.left;
                        tw = bounds.width - insets.left - insets.right - tabAreaInsets.left - tabAreaInsets.right;
                        th = calculateTabAreaHeight(tabPlacement, runCount, maxTabHeight);
                        cx = insets.left + contentInsets.left;
                        cy = insets.top + th + contentInsets.top + tabAreaInsets.top + tabAreaInsets.bottom;
                        cw = bounds.width - insets.left - insets.right - contentInsets.left - contentInsets.right;
                        ch = bounds.height - th - insets.top - insets.bottom - contentInsets.top - contentInsets.bottom
                                - tabAreaInsets.top - tabAreaInsets.bottom;
                        break;
                }

                tabAreaBounds.setRect(tx, ty, tw, th);
                for (int i = 0; i < numChildren; i++) {
                    Component child = tabPane.getComponent(i);
                    if (child == tabContainer) {
                        child.setBounds(tx, ty, tw, th);
                    } else {
                        int tabHeight = maxTabHeight + tabAreaInsets.top + tabAreaInsets.bottom;
                        int tabWidth = maxTabWidth + tabAreaInsets.left + tabAreaInsets.right;
                        int compHeight = ch;
                        int compY = cy;
                        if (northComp != null) {
                            int nh = northComp.getPreferredSize().height;
                            compY -= nh;
                            compHeight += nh;
                        }
                        if (southComp != null) {
                            compHeight += southComp.getPreferredSize().height;
                        }
                        if (child == leadingComp && leadingComp != null) {
                            layoutLeadingComponent(child, tabWidth, tabHeight, insets, tx, ty, tabPlacement);
                        } else if (child == trailingComp && trailingComp != null) {
                            layoutTrailingComponent(child, tabWidth, tabHeight, insets, tx, ty, tw, th, tabPlacement);
                        } else if (child == northComp && northComp != null) {
                            northComp.setBounds(cx, cy - northComp.getPreferredSize().height,
                                                cw, northComp.getPreferredSize().height);
                        } else if (child == southComp && southComp != null) {
                            southComp.setBounds(cx, cy + ch, cw, southComp.getPreferredSize().height);
                        } else if (child == eastComp && eastComp != null) {
                            eastComp.setBounds(cx + cw, compY, eastComp.getPreferredSize().width, compHeight);
                        } else if (child == westComp && westComp != null) {
                            westComp.setBounds(cx - westComp.getPreferredSize().width, compY,
                                               westComp.getPreferredSize().width, compHeight);
                        } else {
                            child.setBounds(cx, cy, cw, ch);
                        }
                    }
                }
            }
            layoutTabComponents();
            if (shouldChangeFocus) {
                if (!requestFocusForVisibleComponent()) {
                    tabPane.requestFocus();
                }
            }
        }
    }

    public class DarkHandler extends Handler {

        @Override
        public void propertyChange(final PropertyChangeEvent e) {
            super.propertyChange(e);
            var key = e.getPropertyName();
            if ("TabbedPane.maxPopupHeight".equals(key)) {
                Integer newVal = (Integer) e.getNewValue();
                if (newVal != null && newVal >= 0) {
                    scrollableTabSupport.scrollPopupMenu.setMaxHeight(newVal);
                }
            } else if ("JTabbedPane.tabAreaInsets".equals(key)) {
                var ins = e.getNewValue();
                if (ins instanceof Insets) {
                    tabAreaInsets = (Insets) ins;
                } else if (ins == null) {
                    tabAreaInsets = new Insets(0, 0, 0, 0);
                }
            } else if ("JTabbedPane.contentBorderInsets".equals(key)) {
                var ins = e.getNewValue();
                if (ins instanceof Insets) {
                    contentBorderInsets = (Insets) ins;
                } else if (ins == null) {
                    contentBorderInsets = new Insets(0, 0, 0, 0);
                }
            } else if ("tabPlacement".equals(key)) {
                if (scrollableTabLayoutEnabled()) {
                    currentShiftX = 0;
                    currentShiftY = 0;
                    scrollLayout.calculateTabRects(tabPane.getTabPlacement(), tabPane.getTabCount());
                }
            } else if ("JTabbedPane.showNewTabButton".equals(key)) {
                var val = e.getNewValue();
                if (val instanceof Boolean && scrollableTabLayoutEnabled()) {
                    boolean show = (Boolean) val;
                    if (show == scrollableTabSupport.newTabButton.isVisible()) {
                        return;
                    }
                    scrollableTabSupport.newTabButton.setVisible(show);
                }
            } else if ("JTabbedPane.leadingComponent".equals(key)) {
                tabPane.remove(leadingComp);
                var val = e.getNewValue();
                if (val instanceof Component) {
                    leadingComp = (Component) val;
                    tabPane.add(leadingComp);
                } else {
                    leadingComp = null;
                }
            } else if ("JTabbedPane.trailingComponent".equals(key)) {
                tabPane.remove(trailingComp);
                var val = e.getNewValue();
                if (val instanceof Component) {
                    trailingComp = (Component) val;
                    tabPane.add(trailingComp);
                } else {
                    trailingComp = null;
                }
            } else if ("JTabbedPane.dndEnabled".equals(key)) {
                dndEnabled = Boolean.TRUE.equals(tabPane.getClientProperty("JTabbedPane.dndEnabled"));
                tabPane.getDropTarget().setActive(dndEnabled);
            } else if ("componentOrientation".equals(key)) {
                tabPane.doLayout();
                tabPane.repaint();
            } else if ("JTabbedPane.northComponent".equals(key)) {
                tabPane.remove(northComp);
                var val = e.getNewValue();
                if (val instanceof Component) {
                    northComp = (Component) val;
                    tabPane.add(northComp);
                } else {
                    northComp = null;
                }
            } else if ("JTabbedPane.southComponent".equals(key)) {
                tabPane.remove(southComp);
                var val = e.getNewValue();
                if (val instanceof Component) {
                    southComp = (Component) val;
                    tabPane.add(southComp);
                } else {
                    southComp = null;
                }
            } else if ("JTabbedPane.eastComponent".equals(key)) {
                tabPane.remove(eastComp);
                var val = e.getNewValue();
                if (val instanceof Component) {
                    eastComp = (Component) val;
                    tabPane.add(eastComp);
                } else {
                    eastComp = null;
                }
            } else if ("JTabbedPane.westComponent".equals(key)) {
                tabPane.remove(westComp);
                var val = e.getNewValue();
                if (val instanceof Component) {
                    westComp = (Component) val;
                    tabPane.add(westComp);
                } else {
                    westComp = null;
                }
            }
        }

        public void stateChanged(@NotNull final ChangeEvent e) {
            JTabbedPane tabPane = (JTabbedPane) e.getSource();
            tabPane.revalidate();
            tabPane.repaint();
            setFocusIndex(tabPane.getSelectedIndex(), false);
        }

        @Override
        public void mousePressed(final MouseEvent e) {
            super.mousePressed(e);
            tabPane.requestFocus();
        }

        @Override
        public void mouseDragged(final MouseEvent e) {
            super.mouseDragged(e);
            if (!dndEnabled) return;

            int index = tabForCoordinate(tabPane, e.getX(), e.getY());
            if (index >= 0 && index < tabPane.getTabCount()) {
                TransferHandler handler = tabPane.getTransferHandler();
                handler.exportAsDrag(tabPane, e, TransferHandler.MOVE);
            }
        }

        @Override
        public void componentAdded(@NotNull final ContainerEvent e) {
            if (!(e.getChild() instanceof UIResource)) {
                e.getChild().addFocusListener(focusListener);
            }
            super.componentAdded(e);
        }

        @Override
        public void componentRemoved(@NotNull final ContainerEvent e) {
            if (!(e.getChild() instanceof UIResource)) {
                e.getChild().removeFocusListener(focusListener);
            }
            super.componentRemoved(e);
        }
    }

    public class DarkScrollHandler extends DarkHandler {

        @Override
        public void mousePressed(@NotNull final MouseEvent e) {
            scrollableTabSupport.timer.stop();
            super.mousePressed(convertEvent(e));
            scrollableTabSupport.endScroll();
        }

        @Override
        public void mouseDragged(final MouseEvent e) {
            super.mouseDragged(convertEvent(e));
        }

        @Override
        public void mouseClicked(final MouseEvent e) {
            super.mouseClicked(convertEvent(e));
        }

        @Override
        public void mouseReleased(final MouseEvent e) {
            super.mouseReleased(convertEvent(e));
        }

        @Override
        public void mouseEntered(final MouseEvent e) {
            super.mouseEntered(convertEvent(e));
        }

        @Override
        public void mouseExited(final MouseEvent e) {
            super.mouseExited(convertEvent(e));
        }

        @Override
        public void mouseMoved(final MouseEvent e) {
            super.mouseMoved(convertEvent(e));
        }
    }
}
