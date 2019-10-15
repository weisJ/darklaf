package com.weis.darklaf.ui.tabbedpane;

import com.weis.darklaf.components.UIResourceWrapper;
import com.weis.darklaf.defaults.DarkColors;
import com.weis.darklaf.defaults.DarkIcons;
import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.GraphicsContext;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.util.TooManyListenersException;

/**
 * @author Jannis Weis
 */
public class DarkTabbedPaneUI extends DarkTabbedPaneUIBridge {

    protected static final TabbedPaneTransferHandler TRANSFER_HANDLER = new TabbedPaneTransferHandler();
    protected final FocusListener focusListener = new FocusListener() {
        @Override
        public void focusGained(final FocusEvent e) {
            repaintTab(tabPane.getSelectedIndex());
        }

        @Override
        public void focusLost(final FocusEvent e) {
            repaintTab(tabPane.getSelectedIndex());
        }
    };
    protected final Rectangle tabAreaBounds = new Rectangle(0, 0, 0, 0);
    protected final Rectangle dropRect = new Rectangle(0, 0, 0, 0);
    protected DarkScrollableTabSupport scrollableTabSupport;
    protected DarkTabbedPaneScrollLayout scrollLayout;
    protected int currentShiftX = 0;
    protected int currentShiftXTmp = 0;
    protected int scrollShiftX = 0;
    protected int currentShiftY = 0;
    protected int currentShiftYTmp = 0;
    protected int scrollShiftY = 0;
    protected int minVisibleOld = -2;
    protected int maxVisibleOld = -2;
    protected int minVisible = -1;
    protected int maxVisible = -1;
    protected boolean dndEnabled;
    protected int dropTargetIndex = -1;
    protected int dropSourceIndex = -1;
    protected boolean sourceEqualsTarget;
    protected boolean drawDropRect;

    protected boolean dragging;
    protected Rectangle dragRect = new Rectangle();


    protected DarkScrollHandler scrollHandler;
    protected Component leadingComp;
    protected Component trailingComp;
    protected Component northComp;
    protected Component eastComp;
    protected Component southComp;
    protected Component westComp;

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkTabbedPaneUI();
    }

    protected Color getDragBorderColor() {
        return DarkColors.get().getTabbedPaneDragBorderColor();
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
            scrollLayout = new DarkTabbedPaneScrollLayout(this);
            return scrollLayout;
        } else { /* WRAP_TAB_LAYOUT */
            return new DarkTabbedPaneLayout(this);
        }
    }

    @Override
    protected void installComponents() {
        if (scrollableTabLayoutEnabled()) {
            if (tabScroller == null) {
                tabScroller = new DarkScrollableTabSupport(this, tabPane.getTabPlacement());
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
            leadingComp = wrapClientComponent((Component) lead);
            tabPane.add(leadingComp);
        }
        var trail = tabPane.getClientProperty("JTabbedPane.trailingComponent");
        if (trail instanceof Component) {
            trailingComp = wrapClientComponent((Component) trail);
            tabPane.add(trailingComp);
        }
        var north = tabPane.getClientProperty("JTabbedPane.northComponent");
        if (north instanceof Component) {
            northComp = wrapClientComponent((Component) north);
            tabPane.add(northComp);
        }
        var south = tabPane.getClientProperty("JTabbedPane.southComponent");
        if (south instanceof Component) {
            southComp = wrapClientComponent((Component) south);
            tabPane.add(southComp);
        }
        var west = tabPane.getClientProperty("JTabbedPane.westComponent");
        if (west instanceof Component) {
            westComp = wrapClientComponent((Component) west);
            tabPane.add(westComp);
        }
        var east = tabPane.getClientProperty("JTabbedPane.eastComponent");
        if (east instanceof Component) {
            eastComp = wrapClientComponent((Component) east);
            tabPane.add(eastComp);
        }
        dndEnabled = Boolean.TRUE.equals(tabPane.getClientProperty("JTabbedPane.dndEnabled"));
    }

    protected Component wrapClientComponent(final Component component) {
        if (component instanceof UIResource) {
            return component;
        }
        return new UIResourceWrapper(component);
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
    protected TabbedPaneHandler getHandler() {
        if (handler == null) {
            handler = new DarkHandler(this);
        }
        return handler;
    }

    @Override
    public void setRolloverTab(final int index) {
        if (dragging) return;
        int oldRollover = rolloverTabIndex;
        super.setRolloverTab(index);
        if (oldRollover != getRolloverTab()) {
            repaintTab(oldRollover);
            repaintTab(getRolloverTab());
        }
    }

    @Override
    protected void paintFocusIndicator(final Graphics g, final int tabPlacement,
                                       final Rectangle[] rects, final int tabIndex,
                                       final Rectangle iconRect, final Rectangle textRect,
                                       final boolean isSelected) {
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

    @Override
    protected void paintContentBorder(final Graphics g, final int tabPlacement, final int selectedIndex) {

    }

    @Override
    protected void paintFocusIndicator(final Graphics g, final int tabPlacement, final Rectangle r,
                                       final int tabIndex, final Rectangle iconRect,
                                       final Rectangle textRect, final boolean isSelected) {
        if (isSelected) {
            if (!drawFocusBar()) return;
            g.setColor(getAccentColor());
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

    protected Color getAccentColor() {
        boolean focus = DarkUIUtil.hasFocus(tabPane);
        return getAccentColor(focus);
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
        g.fillRect(x, y, w, h);
    }

    protected Color getDropColor() {
        if (scrollableTabLayoutEnabled()) {
            return DarkColors.get().getTabbedPaneDropBackground();
        } else {
            return getTabBackgroundColor(0, false, true);
        }
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

    protected Color getTabBackgroundColor(final int tabIndex, final boolean isSelected, final boolean hover) {
        if (isSelected) {
            return hover ? DarkColors.get().getTabbedPaneTabSelectedHoverBackground()
                         : DarkColors.get().getTabbedPaneTabSelectedBackground();
        } else {
            return hover ? DarkColors.get().getTabbedPaneTabHoverBackground()
                         : tabPane.getBackgroundAt(tabIndex);
        }
    }

    protected boolean drawFocusBar() {
        return !Boolean.FALSE.equals(tabPane.getClientProperty("JTabbedPane.drawFocusBar"));
    }

    @Override
    protected int calculateTabHeight(final int tabPlacement, final int tabIndex, final int fontHeight) {
        return super.calculateTabHeight(tabPlacement, tabIndex, fontHeight) - 1;
    }

    protected Color getTabBorderColor() {
        return DarkColors.get().getTabbedPaneTabBorderColor();
    }

    protected DarkScrollHandler getScrollHandler() {
        if (scrollHandler == null) {
            scrollHandler = new DarkScrollHandler(this);
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

    protected Color getAccentColor(final boolean focus) {
        return focus ? DarkColors.get().getTabbedPaneFocusAccentColor()
                     : DarkColors.get().getTabbedPaneAccentColor();
    }

    protected Icon getMoreTabsIcon() {
        return DarkIcons.get().getTabbedPaneMoreTabs();
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

    protected void layoutTabComponents() {
        if (tabContainer == null) {
            return;
        }
        Rectangle rect = new Rectangle();
        Point delta = new Point(-tabContainer.getX(), -tabContainer.getY());
        if (scrollableTabLayoutEnabled()) {
            translatePointToTabPanel(0, 0, delta);
        }
        for (int i = 0; i < tabPane.getTabCount(); i++) {
            Component c = tabPane.getTabComponentAt(i);
            if (c == null) {
                continue;
            }
            getTabBounds(i, rect);

            //Adjust dragged component position.
            if (i == dropSourceIndex) {
                if (dragging) {
                    rect.setBounds(dragRect);
                    var p = rect.getLocation();
                    var vl = tabScroller.viewport.getLocation();
                    p.x += vl.x;
                    p.y += vl.y;
                    rect.setLocation(p);
                } else {
                    rect.setBounds(0, 0, 0, 0);
                }
            }

            Dimension preferredSize = c.getPreferredSize();
            Insets insets = getTabInsets(tabPane.getTabPlacement(), i);
            int outerX = rect.x + insets.left + delta.x;
            int outerY = rect.y + insets.top + delta.y;
            int outerWidth = rect.width - insets.left - insets.right;
            int outerHeight = rect.height - insets.top - insets.bottom;
            //centralize component
            int x = outerX + (outerWidth - preferredSize.width) / 2;
            int y = outerY + (outerHeight - preferredSize.height) / 2;
            int tabPlacement = tabPane.getTabPlacement();
            boolean isSeleceted = i == tabPane.getSelectedIndex();
            c.setBounds(x + getTabLabelShiftX(tabPlacement, i, isSeleceted),
                        y + getTabLabelShiftY(tabPlacement, i, isSeleceted),
                        preferredSize.width, preferredSize.height);
        }
    }

    public Icon getNewTabIcon() {
        return DarkIcons.get().getTabbedPaneNewTabs();
    }

    public JComponent createNewTabButton() {
        return new NewTabButton(this);
    }

    public JButton createMoreTabsButton() {
        return new MoreTabsButton(this);
    }

}
