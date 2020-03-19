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
package com.github.weisj.darklaf.ui.tabbedpane;

import com.github.weisj.darklaf.components.uiresource.UIResourceWrapper;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.GraphicsContext;
import com.github.weisj.darklaf.util.GraphicsUtil;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.event.AWTEventListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.util.TooManyListenersException;
import java.util.function.Consumer;

/**
 * @author Jannis Weis
 */
public class DarkTabbedPaneUI extends DarkTabbedPaneUIBridge {

    protected static final String KEY_PREFIX = "JTabbedPane.";
    public static final String KEY_DND = KEY_PREFIX + "dndEnabled";
    public static final String KEY_NORTH_COMP = KEY_PREFIX + "northComponent";
    public static final String KEY_WEST_COMP = KEY_PREFIX + "westComponent";
    public static final String KEY_EAST_COMP = KEY_PREFIX + "eastComponent";
    public static final String KEY_SOUTH_COMP = KEY_PREFIX + "southComponent";
    public static final String KEY_LEADING_COMP = KEY_PREFIX + "leadingComponent";
    public static final String KEY_TRAILING_COMP = KEY_PREFIX + "trailingComponent";
    public static final String KEY_SHOW_NEW_TAB_BUTTON = KEY_PREFIX + "showNewTabButton";
    public static final String KEY_DRAW_FOCUS_BAR = KEY_PREFIX + "drawFocusBar";
    public static final String KEY_TAB_PLACEMENT = "tabPlacement";
    public static final String KEY_CONTENT_BORDER_INSETS = KEY_PREFIX + "contentBorderInsets";
    public static final String KEY_TAB_AREA_INSETS = KEY_PREFIX + "tabAreaInsets";
    public static final String KEY_MAX_POPUP_HEIGHT = KEY_PREFIX + "maxPopupHeight";
    public static final String KEY_NEW_TAB_ACTION = KEY_PREFIX + "newTabAction";
    public static final String KEY_ROTATE_TAB_RUNS = KEY_PREFIX + "rotateTabRuns";

    protected static final TabbedPaneTransferHandler TRANSFER_HANDLER = new TabbedPaneTransferHandler.UIResource();
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
    protected final AWTEventListener awtEventListener = e -> {
        if (e.getID() == FocusEvent.FOCUS_GAINED) {
            tabPane.repaint();
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
    protected int focusSize;

    protected boolean dragging;
    protected Rectangle dragRect = new Rectangle();

    protected Color dragBorderColor;
    protected Color dropBackground;
    protected Color tabBorderColor;
    protected Color accent;
    protected Color focusAccent;
    protected Color selectedHoverBackground;
    protected Color selectedBackground;
    protected Color hoverBackground;
    protected Color tabAreaBackground;

    protected Icon moreTabsIcon;
    protected Icon newTabIcon;


    protected DarkScrollHandler scrollHandler;
    protected Component leadingComp;
    protected Component trailingComp;
    protected Component northComp;
    protected Component eastComp;
    protected Component southComp;
    protected Component westComp;


    public static ComponentUI createUI(final JComponent c) {
        return new DarkTabbedPaneUI();
    }

    protected Color getDragBorderColor() {
        return dragBorderColor;
    }

    protected Action getNewTabAction() {
        Object action = tabPane.getClientProperty(KEY_NEW_TAB_ACTION);
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
        if (scrollableTabSupport != null) {
            tabPane.remove(scrollableTabSupport.moreTabsButton);
            tabPane.remove(scrollableTabSupport.newTabButton);
        }
        scrollableTabSupport = null;
        if (tabPane.getTransferHandler() instanceof TabbedPaneTransferHandler.UIResource) {
            tabPane.setTransferHandler(null);
            if (tabPane.getDropTarget() != null) {
                tabPane.getDropTarget().removeDropTargetListener(TRANSFER_HANDLER);
                tabPane.getDropTarget().setActive(false);
            }
        }
        super.uninstallUI(c);
    }

    public void paint(final Graphics g, final JComponent c) {
        int selectedIndex = tabPane.getSelectedIndex();
        int tabPlacement = tabPane.getTabPlacement();

        ensureCurrentLayout();

        // Paint content border and tab area
        if (tabsOverlapBorder) {
            paintContentBorder(g, tabPlacement, selectedIndex);
        }


        // If scrollable tabs are enabled, the tab area will be
        // painted by the scrollable tab panel instead.
        if (!scrollableTabLayoutEnabled()) { // WRAP_TAB_LAYOUT
            paintTabArea(g, tabPlacement, selectedIndex);
        } else {
            paintTabAreaBorder(g, tabPlacement);
        }

        if (!tabsOverlapBorder) {
            paintContentBorder(g, tabPlacement, selectedIndex);
        }

        if (!scrollableTabLayoutEnabled() && drawDropRect) {
            paintDrop(g);
        }

        if (tabPane.getTabCount() == 0) {
            paintTabAreaBorder(g, tabPlacement);
        }
    }

    @Override
    protected void paintContentBorder(final Graphics g, final int tabPlacement, final int selectedIndex) {

    }

    @Override
    protected void paintTabArea(final Graphics g, final int tabPlacement, final int selectedIndex) {
        paintTabAreaBackground(g, tabPlacement);
        paintTabAreaBorder(g, tabPlacement);
        super.paintTabArea(g, tabPlacement, selectedIndex);
    }

    @Override
    protected void paintTabBackground(final Graphics g, final int tabPlacement, final int tabIndex,
                                      final int x,
                                      final int y, final int w, final int h,
                                      final boolean isSelected) {
        g.setColor(getTabBackgroundColor(tabIndex, isSelected, getRolloverTab() == tabIndex));
        g.fillRect(x, y, w, h);
    }

    @Override
    protected void paintTabBorder(final Graphics g, final int tabPlacement, final int tabIndex,
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
    protected void paintFocusIndicator(final Graphics g, final int tabPlacement, final Rectangle r,
                                       final int tabIndex, final Rectangle iconRect,
                                       final Rectangle textRect, final boolean isSelected) {
        if (isSelected) {
            if (!drawFocusBar()) return;
            g.setColor(getAccentColor());
            switch (tabPlacement) {
                case LEFT:
                    g.fillRect(r.x + r.width - focusSize, r.y, focusSize, r.height);
                    break;
                case RIGHT:
                    g.fillRect(r.x, r.y, focusSize, r.height);
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

    protected void paintTabAreaBackground(final Graphics g, final int tabPlacement) {
        g.setColor(getTabAreaBackground());
        Rectangle b = getTabAreaBounds();
        if (scrollableTabLayoutEnabled()) {
            b.setLocation(0, 0);
        }
        switch (tabPlacement) {
            case BOTTOM:
                b.y++;
            case TOP:
                b.height--;
                break;
            case RIGHT:
                b.x++;
            case LEFT:
                b.width--;
                break;
        }
        g.fillRect(b.x, b.y, b.width, b.height);
    }

    protected Component wrapClientComponent(final Component component) {
        if (component instanceof UIResource) {
            return component;
        }
        return new UIResourceWrapper(component);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        Toolkit.getDefaultToolkit().removeAWTEventListener(awtEventListener);
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
        Toolkit.getDefaultToolkit().addAWTEventListener(awtEventListener, AWTEvent.FOCUS_EVENT_MASK);
    }

    @Override
    protected TabbedPaneHandler getHandler() {
        if (handler == null) {
            handler = new DarkHandler(this);
        }
        return handler;
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

    private void paintTabAreaBorder(final Graphics g, final int tabPlacement) {
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
    }

    protected boolean shouldRotateTabRuns(final int tabPlacement) {
        return Boolean.TRUE.equals(tabPane.getClientProperty(KEY_ROTATE_TAB_RUNS));
    }

    @Override
    protected int calculateTabHeight(final int tabPlacement, final int tabIndex, final int fontHeight) {
        return super.calculateTabHeight(tabPlacement, tabIndex, fontHeight) - 1;
    }

    @Override
    protected int calculateMaxTabHeight(final int tabPlacement) {
        return Math.max(super.calculateMaxTabHeight(tabPlacement), getFallBackSize());
    }

    @Override
    protected int calculateMaxTabWidth(final int tabPlacement) {
        return Math.max(super.calculateMaxTabWidth(tabPlacement), getFallBackSize());
    }

    @Override
    protected int calculateTabAreaHeight(final int tabPlacement, final int horizRunCount, final int maxTabHeight) {
        return Math.max(super.calculateTabAreaHeight(tabPlacement, horizRunCount, maxTabHeight), getFallBackSize());
    }

    @Override
    protected int calculateTabAreaWidth(final int tabPlacement, final int vertRunCount, final int maxTabWidth) {
        return Math.max(super.calculateTabAreaWidth(tabPlacement, vertRunCount, maxTabWidth), getFallBackSize());
    }

    public void setDnDIndicatorRect(final int x, final int y, final int width, final int height, final int targetIndex,
                                    final boolean sourceEqualsTarget) {
        dropRect.setBounds(x, y, width, height);
        if (scrollableTabLayoutEnabled()) {
            Point p = scrollableTabSupport.viewport.getLocation();
            dropRect.x -= p.x;
            dropRect.y -= p.y;
        }
        drawDropRect = true;
        this.sourceEqualsTarget = sourceEqualsTarget;
        dropTargetIndex = targetIndex;
        tabPane.doLayout();
        tabPane.repaint();
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

    protected int getFallBackSize() {
        int max = 0;
        if (scrollableTabLayoutEnabled() && scrollableTabSupport.newTabButton.isVisible()) {
            max = Math.max(scrollableTabSupport.newTabButton.getPreferredSize().height, 27);
        }
        return max;
    }

    protected DarkScrollHandler getScrollHandler() {
        if (scrollHandler == null) {
            scrollHandler = new DarkScrollHandler(this);
        }
        return scrollHandler;
    }

    protected boolean drawFocusBar() {
        return !Boolean.FALSE.equals(tabPane.getClientProperty(KEY_DRAW_FOCUS_BAR));
    }

    protected Color getAccentColor() {
        boolean focus = DarkUIUtil.hasFocus(tabPane);
        return getAccentColor(focus);
    }

    protected Color getAccentColor(final boolean focus) {
        return focus ? focusAccent : accent;
    }

    protected Color getTabBorderColor() {
        return tabBorderColor;
    }

    public Color getTabBackgroundColor(final int tabIndex, final boolean isSelected, final boolean hover) {
        if (isSelected) {
            return hover ? selectedHoverBackground : selectedBackground;
        } else {
            return hover ? hoverBackground : tabPane.getBackgroundAt(tabIndex);
        }
    }

    protected void paintTabAreaBorder(final Graphics g, final int tabPlacement,
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

    protected Color getTabAreaBackground() {
        return tabAreaBackground;
    }

    protected void paintDrop(final Graphics g) {
        g.setColor(getDropColor());
        GraphicsContext context = new GraphicsContext(g);
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

    protected void layoutLeadingComponent(final Component comp, final int tabWidth, final int tabHeight,
                                          final Insets insets, final int tx, final int ty, final int tabPlacement) {
        Dimension b = leadingComp.getPreferredSize();
        int h = Math.min(tabHeight, b.height);
        int w = Math.min(tabWidth, b.width);
        int centerY = (tabHeight - h) / 2;
        int centerX = (tabWidth - w) / 2;
        switch (tabPlacement) {
            case LEFT:
                comp.setBounds(insets.left + centerX, insets.top, w, b.height);
                break;
            case RIGHT:
                comp.setBounds(tx - tabAreaInsets.left + centerX, insets.top,
                               w, b.height);
                break;
            case TOP:
                comp.setBounds(insets.left, insets.top + centerY, b.width, h);
                break;
            case BOTTOM:
                comp.setBounds(insets.left, ty - tabAreaInsets.bottom + centerY,
                               b.width, h);
                break;
        }
    }

    protected void layoutTrailingComponent(final Component comp, final int tabWidth, final int tabHeight,
                                           final Insets insets, final int tx, final int ty,
                                           final int tw, final int th, final int tabPlacement) {
        Dimension b = trailingComp.getPreferredSize();
        int h = Math.min(tabHeight, b.height);
        int w = Math.min(tabWidth, b.width);
        Dimension size = tabPane.getSize();
        int centerY = (tabHeight - h) / 2;
        int centerX = (tabWidth - w) / 2;
        switch (tabPlacement) {
            case LEFT:
                comp.setBounds(insets.left + centerX, size.height - b.height - insets.bottom,
                               w, b.height);
                break;
            case RIGHT:
                comp.setBounds(tx - tabAreaInsets.left + centerX, size.height - b.height - insets.bottom,
                               w, b.height);
                break;
            case TOP:
                comp.setBounds(size.width - b.width - insets.right, insets.top + centerY, b.width, h);
                break;
            case BOTTOM:
                comp.setBounds(size.width - b.width - insets.right, ty - tabAreaInsets.bottom + centerY,
                               b.width, h);
                break;
        }
    }

    protected Color getDropColor() {
        if (scrollableTabLayoutEnabled()) {
            return dropBackground;
        } else {
            return getTabBackgroundColor(0, false, true);
        }
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

    protected MouseEvent convertEvent(final MouseEvent e) {
        Point p = e.getPoint();
        Point pos = scrollableTabSupport.viewport.getLocation();
        p.x += pos.x;
        p.y += pos.y;
        return new MouseEvent(e.getComponent(), e.getID(), e.getWhen(), e.getModifiersEx(),
                              p.x, p.y, e.getClickCount(), e.isPopupTrigger(), e.getButton());
    }

    public Rectangle getTabAreaBounds() {
        if (scrollableTabLayoutEnabled()) {
            return scrollableTabSupport.viewport.getBounds();
        } else {
            return new Rectangle(tabAreaBounds);
        }
    }

    protected Icon getMoreTabsIcon() {
        return moreTabsIcon;
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
                    Point p = rect.getLocation();
                    Point vl = tabScroller.viewport.getLocation();
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

    @Override
    protected void paintText(final Graphics g, final int tabPlacement, final Font font,
                             final FontMetrics metrics, final int tabIndex, final String title,
                             final Rectangle textRect, final boolean isSelected) {
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        super.paintText(g, tabPlacement, font, metrics, tabIndex, title, textRect, isSelected);
        config.restore();
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        dragBorderColor = UIManager.getColor("TabbedPane.dragBorderColor");
        dropBackground = UIManager.getColor("TabbedPane.dropFill");
        tabBorderColor = UIManager.getColor("TabbedPane.tabBorderColor");
        accent = UIManager.getColor("TabbedPane.accent");
        focusAccent = UIManager.getColor("TabbedPane.accentFocus");
        selectedHoverBackground = UIManager.getColor("TabbedPane.selectedHoverBackground");
        selectedBackground = UIManager.getColor("TabbedPane.selectedBackground");
        hoverBackground = UIManager.getColor("TabbedPane.hoverBackground");
        tabAreaBackground = UIManager.getColor("TabbedPane.tabAreaBackground");

        focusSize = UIManager.getInt("TabbedPane.focusBarHeight");
        moreTabsIcon = UIManager.getIcon("TabbedPane.moreTabs.icon");
        newTabIcon = UIManager.getIcon("TabbedPane.newTab.icon");

        Object ins = tabPane.getClientProperty(KEY_TAB_AREA_INSETS);
        if (ins instanceof Insets) {
            tabAreaInsets = (Insets) ins;
        }
        ins = tabPane.getClientProperty(KEY_CONTENT_BORDER_INSETS);
        if (ins instanceof Insets) {
            contentBorderInsets = (Insets) ins;
        }
        installComponent(KEY_LEADING_COMP, c -> leadingComp = c);
        installComponent(KEY_TRAILING_COMP, c -> trailingComp = c);
        installComponent(KEY_NORTH_COMP, c -> northComp = c);
        installComponent(KEY_SOUTH_COMP, c -> southComp = c);
        installComponent(KEY_WEST_COMP, c -> westComp = c);
        installComponent(KEY_EAST_COMP, c -> eastComp = c);
        dndEnabled = Boolean.TRUE.equals(tabPane.getClientProperty(KEY_DND));
    }

    protected void installComponent(final String key, final Consumer<Component> setter) {
        Object comp = tabPane.getClientProperty(key);
        if (comp instanceof Component) {
            Component wrapped = wrapClientComponent((Component) comp);
            setter.accept(wrapped);
            tabPane.add(wrapped);
        }
    }

    @Override
    public Rectangle getTabBounds(final JTabbedPane pane, final int i) {
        Rectangle rect = super.getTabBounds(pane, i);
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
    protected Insets getTabAreaInsets(final int tabPlacement) {
        Insets insets = super.getTabAreaInsets(tabPlacement);
        if (leadingComp != null) {
            Dimension b = leadingComp.getPreferredSize();
            if (isHorizontalTabPlacement()) {
                insets.left += b.width;
            } else {
                insets.top += b.height;
            }
        }
        if (trailingComp != null) {
            Dimension b = trailingComp.getPreferredSize();
            if (isHorizontalTabPlacement()) {
                insets.right += b.width;
            } else {
                insets.bottom += b.height;
            }
        }
        return insets;
    }

    public Icon getNewTabIcon() {
        return newTabIcon;
    }

    public JComponent createNewTabButton() {
        return new NewTabButton(this);
    }

    public JButton createMoreTabsButton() {
        return new MoreTabsButton(this);
    }
}
