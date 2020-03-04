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
package com.github.weisj.darklaf.components.tabframe;

import com.github.weisj.darklaf.decorators.AncestorAdapter;
import com.github.weisj.darklaf.utils.Alignment;

import javax.swing.*;
import javax.swing.event.AncestorEvent;
import java.awt.*;
import java.util.function.BiConsumer;

/**
 * Content pane for {@link JTabFrame}.
 *
 * @author Jannis Weis
 */
public class TabFrameContentPane extends JPanel implements TabFrameContent {
    protected static final double HORIZONTAL_PROP_LEFT = 0.2;
    protected static final double VERTICAL_PROP_TOP = 0.2;
    protected static final double VERTICAL_PROP_BOTTOM = 0.75;
    protected static final double HORIZONTAL_PROP_RIGHT = 0.75;

    protected final ToggleSplitPane topSplit;
    protected final ToggleSplitPane bottomSplit;
    protected final ToggleSplitPane leftSplit;
    protected final ToggleSplitPane rightSplit;
    protected final ToggleSplitPane leftSplitter;
    protected final ToggleSplitPane rightSplitter;
    protected final ToggleSplitPane topSplitter;
    protected final ToggleSplitPane bottomSplitter;

    protected final boolean[] enabled = new boolean[8];

    protected Component cont = new JPanel();

    public TabFrameContentPane() {
        super(new BorderLayout());
        cont.setBackground(Color.YELLOW);

        PopupContainer leftBottomPanel = new PopupContainer();
        PopupContainer rightTopPanel = new PopupContainer();
        PopupContainer rightBottomPanel = new PopupContainer();
        PopupContainer topLeftPanel = new PopupContainer();
        PopupContainer topRightPanel = new PopupContainer();
        PopupContainer bottomLeftPanel = new PopupContainer();
        PopupContainer bottomRightPanel = new PopupContainer();
        PopupContainer leftTopPanel = new PopupContainer();

        rightSplitter = createSplitPane("rightSplitter");
        rightSplitter.setOrientation(JSplitPane.VERTICAL_SPLIT);
        rightSplitter.setTopComponent(rightTopPanel);
        rightSplitter.setBottomComponent(rightBottomPanel);

        leftSplitter = createSplitPane("leftSplitter");
        leftSplitter.setOrientation(JSplitPane.VERTICAL_SPLIT);
        leftSplitter.setTopComponent(leftTopPanel);
        leftSplitter.setBottomComponent(leftBottomPanel);

        topSplitter = createSplitPane("topSplitter");
        topSplitter.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
        topSplitter.setLeftComponent(topLeftPanel);
        topSplitter.setRightComponent(topRightPanel);

        bottomSplitter = createSplitPane("bottomSplitter");
        bottomSplitter.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
        bottomSplitter.setLeftComponent(bottomLeftPanel);
        bottomSplitter.setRightComponent(bottomRightPanel);


        topSplit = createSplitPane("topSplit");
        bottomSplit = createSplitPane("bottomSplit");
        topSplit.setOrientation(JSplitPane.VERTICAL_SPLIT);
        bottomSplit.setOrientation(JSplitPane.VERTICAL_SPLIT);

        topSplit.setTopComponent(topSplitter);
        topSplit.setBottomComponent(bottomSplit);
        bottomSplit.setBottomComponent(bottomSplitter);

        leftSplit = createSplitPane("leftSplit");
        rightSplit = createSplitPane("rightSplit");
        leftSplit.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
        rightSplit.setOrientation(JSplitPane.HORIZONTAL_SPLIT);

        bottomSplit.setTopComponent(leftSplit);
        leftSplit.setLeftComponent(leftSplitter);
        leftSplit.setRightComponent(rightSplit);
        rightSplit.setRightComponent(rightSplitter);
        rightSplit.setLeftComponent(cont);

        topSplit.setResizeWeight(0.0d);
        leftSplit.setResizeWeight(0.0d);
        bottomSplit.setResizeWeight(1.0d);
        rightSplit.setResizeWeight(1.0d);
        setupSplitterPanes(JSplitPane::setResizeWeight, 0.5d);

        setupSplitPanes(JSplitPane::setEnabled, false);
        setupSplitPanes(ToggleSplitPane::setResizable, false);
        setupSplitterPanes(JSplitPane::setEnabled, false);
        setupSplitterPanes(ToggleSplitPane::setResizable, false);

        add(topSplit, BorderLayout.CENTER);

        addAncestorListener(new AncestorAdapter() {
            @Override
            public void ancestorAdded(final AncestorEvent event) {
                removeAncestorListener(this);
                SwingUtilities.invokeLater(() -> init());
            }
        });
    }

    protected ToggleSplitPane createSplitPane(final String name) {
        return new TabFrameSplitPane(name);
    }

    protected <T> void setupSplitterPanes(final BiConsumer<? super ToggleSplitPane, T> consumer,
                                          final T flag) {
        consumer.accept(bottomSplitter, flag);
        consumer.accept(leftSplitter, flag);
        consumer.accept(rightSplitter, flag);
        consumer.accept(topSplitter, flag);
    }

    protected <T> void setupSplitPanes(final BiConsumer<? super ToggleSplitPane, T> consumer,
                                       final T flag) {
        consumer.accept(topSplit, flag);
        consumer.accept(leftSplit, flag);
        consumer.accept(bottomSplit, flag);
        consumer.accept(rightSplit, flag);
    }

    private void init() {
        disableAll(true);
        topSplit.savePosition(VERTICAL_PROP_TOP);
        bottomSplit.savePosition(VERTICAL_PROP_BOTTOM);
        leftSplit.savePosition(HORIZONTAL_PROP_LEFT);
        rightSplit.savePosition(HORIZONTAL_PROP_RIGHT);
        setupSplitterPanes(ToggleSplitPane::savePosition, 0.5d);
        doLayout();
    }

    public void disableAll(final boolean force) {
        for (Alignment a : Alignment.values()) {
            setEnabled(a, false, force);
        }
    }

    /**
     * Show or hide the corresponding panel.
     *
     * @param a       position of panel.
     * @param enabled true if should be shown.
     */
    public void setEnabled(final Alignment a, final boolean enabled) {
        setEnabled(a, enabled, false);
    }

    /**
     * Show or hide the corresponding panel.
     *
     * @param a       position of panel.
     * @param enabled true if should be shown.
     * @param force   whether to force the layout process.
     */
    public void setEnabled(final Alignment a, final boolean enabled, final boolean force) {
        if (enabled == isEnabled(a) && !force) return;
        switch (a) {
            case NORTH:
                changeStatus(
                        enabled, Alignment.NORTH_EAST,
                        topSplit, topSplitter,
                        new LayoutProportions(VERTICAL_PROP_TOP, 1.0, 0.0, 0.0),
                        new LayoutWeights(0.0, 0.0, 0.0, 1.0));
                break;
            case NORTH_EAST:
                changeStatus(
                        enabled, Alignment.NORTH,
                        topSplit, topSplitter,
                        new LayoutProportions(VERTICAL_PROP_TOP, 0.0, 0.0, 1.0),
                        new LayoutWeights(0.0, 0.0, 1.0, 0.0));
                break;
            case EAST:
                changeStatus(
                        enabled, Alignment.SOUTH_EAST,
                        rightSplit, rightSplitter,
                        new LayoutProportions(HORIZONTAL_PROP_RIGHT, 1.0, 1.0, 0.0),
                        new LayoutWeights(1.0, 1.0, 0.0, 1.0));
                break;
            case SOUTH_EAST:
                changeStatus(
                        enabled, Alignment.EAST,
                        rightSplit, rightSplitter,
                        new LayoutProportions(HORIZONTAL_PROP_RIGHT, 0.0, 1.0, 1.0),
                        new LayoutWeights(1.0, 1.0, 1.0, 0.0));
                break;
            case NORTH_WEST:
                changeStatus(
                        enabled, Alignment.WEST,
                        leftSplit, leftSplitter,
                        new LayoutProportions(VERTICAL_PROP_TOP, 1.0, 0.0, 0.0),
                        new LayoutWeights(0.0, 0.0, 0.0, 1.0));
                break;
            case WEST:
                changeStatus(
                        enabled, Alignment.NORTH_WEST,
                        leftSplit, leftSplitter,
                        new LayoutProportions(VERTICAL_PROP_TOP, 0.0, 0.0, 1.0),
                        new LayoutWeights(0.0, 0.0, 1.0, 0.0));
                break;
            case SOUTH_WEST:
                changeStatus(
                        enabled, Alignment.SOUTH,
                        bottomSplit, bottomSplitter,
                        new LayoutProportions(VERTICAL_PROP_BOTTOM, 1.0, 1.0, 0.0),
                        new LayoutWeights(1.0, 1.0, 0.0, 1.0));
                break;
            case SOUTH:
                changeStatus(
                        enabled, Alignment.SOUTH_WEST,
                        bottomSplit, bottomSplitter,
                        new LayoutProportions(VERTICAL_PROP_BOTTOM, 0.0, 1.0, 1.0),
                        new LayoutWeights(1.0, 1.0, 1.0, 0.0));
                break;
            case CENTER:
                break;
        }
        setEnabledFlag(a, enabled);
    }

    /**
     * Change status of panel.
     *
     * @param enabled     new status.
     * @param peer        peer alignment.
     * @param split       the split panel.
     * @param splitter    the splitter panel.
     * @param proportions the layout proportions
     * @param weights     the layout weights
     */
    private void changeStatus(final boolean enabled, final Alignment peer,
                              final ToggleSplitPane split,
                              final ToggleSplitPane splitter,
                              final LayoutProportions proportions,
                              final LayoutWeights weights) {
        if (enabled) {
            enable(split, weights.splitEnable);
            if (!isEnabled(peer)) {
                disable(splitter, weights.splitterPeerDisable, proportions.splitterPeerDisable);
            } else {
                enable(splitter, 0.5d);
            }
        } else {
            if (!isEnabled(peer)) {
                disable(split, weights.splitDisable, proportions.splitDisable);
            }
            disable(splitter, weights.splitterDisable, proportions.splitterDisable);
        }
    }

    /*
     * Update the flags.
     */
    private void setEnabledFlag(final Alignment a, final boolean e) {
        if (a != Alignment.CENTER) {
            enabled[a.getIndex()] = e;
        }
    }

    private void enable(final ToggleSplitPane splitPane, final double weight) {
        boolean restore = !splitPane.isResizable();
        splitPane.setEnabled(true);
        splitPane.setResizable(true);
        splitPane.setResizeWeight(weight);
        if (restore) {
            splitPane.restorePosition();
        }
    }

    public Component getContent() {
        return cont;
    }

    public void setContent(final Component pane) {
        cont = pane;
        rightSplit.setLeftComponent(pane);
    }

    private void disable(final ToggleSplitPane splitPane, final double weight, final double location) {
        if (splitPane.isResizable()) {
            splitPane.savePosition();
        }
        splitPane.setResizeWeight(weight);
        splitPane.forceSetDividerLocation(location);
        splitPane.setEnabled(false);
        splitPane.setResizable(false);
    }

    /**
     * Returns whether the corresponding panel is currently enabled/visible.
     *
     * @param a the position of the panel.
     * @return true if enabled.
     */
    public boolean isEnabled(final Alignment a) {
        if (a == Alignment.CENTER) {
            return false;
        } else {
            return enabled[a.getIndex()];
        }
    }

    public void setComponentAt(final Alignment a, final Component c) {
        switch (a) {
            case NORTH:
                ((PopupContainer) topSplitter.getLeftComponent()).setPopup(c);
                break;
            case NORTH_EAST:
                ((PopupContainer) topSplitter.getRightComponent()).setPopup(c);
                break;
            case EAST:
                ((PopupContainer) rightSplitter.getTopComponent()).setPopup(c);
                break;
            case SOUTH_EAST:
                ((PopupContainer) rightSplitter.getBottomComponent()).setPopup(c);
                break;
            case SOUTH:
                ((PopupContainer) bottomSplitter.getRightComponent()).setPopup(c);
                break;
            case SOUTH_WEST:
                ((PopupContainer) bottomSplitter.getLeftComponent()).setPopup(c);
                break;
            case WEST:
                ((PopupContainer) leftSplitter.getBottomComponent()).setPopup(c);
                break;
            case NORTH_WEST:
                ((PopupContainer) leftSplitter.getTopComponent()).setPopup(c);
                break;
            case CENTER:
                break;
        }
    }

    @Override
    public Component getComponent() {
        return this;
    }

    /**
     * Get the status of the individual panels.
     *
     * @return array of status of panels.
     */

    public boolean[] getStatus() {
        return enabled;
    }

    @Override
    public PopupContainer getContainer(final Alignment alignment) {
        PopupContainer popupComponent;
        switch (alignment) {
            case NORTH:
                popupComponent = ((PopupContainer) topSplitter.getLeftComponent());
                break;
            case NORTH_EAST:
                popupComponent = ((PopupContainer) topSplitter.getRightComponent());
                break;
            case EAST:
                popupComponent = ((PopupContainer) rightSplitter.getTopComponent());
                break;
            case SOUTH_EAST:
                popupComponent = ((PopupContainer) rightSplitter.getBottomComponent());
                break;
            case SOUTH:
                popupComponent = ((PopupContainer) bottomSplitter.getRightComponent());
                break;
            case SOUTH_WEST:
                popupComponent = ((PopupContainer) bottomSplitter.getLeftComponent());
                break;
            case WEST:
                popupComponent = ((PopupContainer) leftSplitter.getBottomComponent());
                break;
            case NORTH_WEST:
                popupComponent = ((PopupContainer) leftSplitter.getTopComponent());
                break;
            default:
                throw new IllegalArgumentException("CENTER is not supported");
        }
        return popupComponent;
    }

    /**
     * Get the popup component at the position.
     *
     * @param a the position.
     * @return the popup component at position.
     */

    public Component getPopupComponent(final Alignment a) {
        return getContainer(a).getPopup();
    }

    protected static class LayoutProportions {
        protected final double splitRestore;
        protected final double splitterPeerDisable;
        protected final double splitDisable;
        protected final double splitterDisable;


        public LayoutProportions(final double splitRestore, final double splitterPeerDisable,
                                 final double splitDisable, final double splitterDisable) {
            this.splitRestore = splitRestore;
            this.splitterPeerDisable = splitterPeerDisable;
            this.splitDisable = splitDisable;
            this.splitterDisable = splitterDisable;
        }
    }

    protected static class LayoutWeights {
        protected final double splitEnable;
        protected final double splitterDisable;
        protected final double splitDisable;
        protected final double splitterPeerDisable;


        public LayoutWeights(final double splitEnable, final double splitterDisable,
                             final double splitDisable, final double splitterPeerDisable) {
            this.splitEnable = splitEnable;
            this.splitterDisable = splitterDisable;
            this.splitDisable = splitDisable;
            this.splitterPeerDisable = splitterPeerDisable;
        }
    }

    protected static class TabFrameSplitPane extends ToggleSplitPane {
        protected TabFrameSplitPane(final String name) {
            super(name);
        }
    }
}
