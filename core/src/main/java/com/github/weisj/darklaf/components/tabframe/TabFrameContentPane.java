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
package com.github.weisj.darklaf.components.tabframe;

import java.awt.*;
import java.util.function.BiConsumer;

import javax.swing.*;

import com.github.weisj.darklaf.util.Alignment;

/**
 * Content pane for {@link JTabFrame}.
 *
 * @author Jannis Weis
 */
public class TabFrameContentPane extends JPanel implements TabFrameContent {
    protected static final double TOP_SPLIT_DEFAULT_POSITION = 0.2;
    protected static final double BOTTOM_SPLIT_DEFAULT_POSITION = calculateRelativeComplement(0.2);
    protected static final double LEFT_SPLIT_DEFAULT_POSITION = 0.2;
    protected static final double RIGHT_SPLIT_DEFAULT_POSITION = calculateRelativeComplement(0.2);
    protected static final double SPLITTER_DEFAULT_POSITION = 0.5;

    private static double calculateRelativeComplement(final double x) {
        // When using two (horizontal) split panes in combination to achieve that the left and right
        // panes have the same relative the following equation needs to be fulfilled:
        //
        // x := relative position of left split pane
        // y := relative position of left split pane
        // x == (1 - x)*(1 - y)
        //
        // thus y = 1 - x / (1 - x)
        return 1 - (x / (1 - x));
    }

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
        init();
    }

    protected ToggleSplitPane createSplitPane(final String name) {
        return new TabFrameSplitPane(name);
    }

    protected <T> void setupSplitterPanes(final BiConsumer<? super ToggleSplitPane, T> consumer, final T flag) {
        consumer.accept(bottomSplitter, flag);
        consumer.accept(leftSplitter, flag);
        consumer.accept(rightSplitter, flag);
        consumer.accept(topSplitter, flag);
    }

    protected <T> void setupSplitPanes(final BiConsumer<? super ToggleSplitPane, T> consumer, final T flag) {
        consumer.accept(topSplit, flag);
        consumer.accept(leftSplit, flag);
        consumer.accept(bottomSplit, flag);
        consumer.accept(rightSplit, flag);
    }

    protected void init() {
        disableAll(true);
        setupSplitterPanes(ToggleSplitPane::savePosition, SPLITTER_DEFAULT_POSITION);
        topSplit.savePosition(TOP_SPLIT_DEFAULT_POSITION);
        bottomSplit.savePosition(BOTTOM_SPLIT_DEFAULT_POSITION);
        leftSplit.savePosition(LEFT_SPLIT_DEFAULT_POSITION);
        rightSplit.savePosition(RIGHT_SPLIT_DEFAULT_POSITION);
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
     * @param a position of panel.
     * @param enabled true if should be shown.
     */
    @Override
    public void setEnabled(final Alignment a, final boolean enabled) {
        setEnabled(a, enabled, false);
    }

    /**
     * Show or hide the corresponding panel.
     *
     * @param a position of panel.
     * @param enabled true if should be shown.
     * @param force whether to force the layout process.
     */
    public void setEnabled(final Alignment a, final boolean enabled, final boolean force) {
        if (enabled == isEnabled(a) && !force) return;
        switch (a) {
            case NORTH:
                changeStatus(enabled, Alignment.NORTH_EAST, topSplit, topSplitter,
                        new LayoutProportions(TOP_SPLIT_DEFAULT_POSITION, 1.0, 0.0, 0.0),
                        new LayoutWeights(0.0, 0.0, 0.0, 1.0));
                break;
            case NORTH_EAST:
                changeStatus(enabled, Alignment.NORTH, topSplit, topSplitter,
                        new LayoutProportions(TOP_SPLIT_DEFAULT_POSITION, 0.0, 0.0, 1.0),
                        new LayoutWeights(0.0, 0.0, 1.0, 0.0));
                break;
            case EAST:
                changeStatus(enabled, Alignment.SOUTH_EAST, rightSplit, rightSplitter,
                        new LayoutProportions(RIGHT_SPLIT_DEFAULT_POSITION, 1.0, 1.0, 0.0),
                        new LayoutWeights(1.0, 1.0, 0.0, 1.0));
                break;
            case SOUTH_EAST:
                changeStatus(enabled, Alignment.EAST, rightSplit, rightSplitter,
                        new LayoutProportions(RIGHT_SPLIT_DEFAULT_POSITION, 0.0, 1.0, 1.0),
                        new LayoutWeights(1.0, 1.0, 1.0, 0.0));
                break;
            case NORTH_WEST:
                changeStatus(enabled, Alignment.WEST, leftSplit, leftSplitter,
                        new LayoutProportions(TOP_SPLIT_DEFAULT_POSITION, 1.0, 0.0, 0.0),
                        new LayoutWeights(0.0, 0.0, 0.0, 1.0));
                break;
            case WEST:
                changeStatus(enabled, Alignment.NORTH_WEST, leftSplit, leftSplitter,
                        new LayoutProportions(TOP_SPLIT_DEFAULT_POSITION, 0.0, 0.0, 1.0),
                        new LayoutWeights(0.0, 0.0, 1.0, 0.0));
                break;
            case SOUTH_WEST:
                changeStatus(enabled, Alignment.SOUTH, bottomSplit, bottomSplitter,
                        new LayoutProportions(BOTTOM_SPLIT_DEFAULT_POSITION, 1.0, 1.0, 0.0),
                        new LayoutWeights(1.0, 1.0, 0.0, 1.0));
                break;
            case SOUTH:
                changeStatus(enabled, Alignment.SOUTH_WEST, bottomSplit, bottomSplitter,
                        new LayoutProportions(BOTTOM_SPLIT_DEFAULT_POSITION, 0.0, 1.0, 1.0),
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
     * @param enabled new status.
     * @param peer peer alignment.
     * @param split the split panel.
     * @param splitter the splitter panel.
     * @param proportions the layout proportions
     * @param weights the layout weights
     */
    private void changeStatus(final boolean enabled, final Alignment peer, final ToggleSplitPane split,
            final ToggleSplitPane splitter, final LayoutProportions proportions, final LayoutWeights weights) {
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

    @Override
    public Component getContent() {
        return cont;
    }

    @Override
    public void setContent(final Component pane) {
        cont = pane;
        rightSplit.setLeftComponent(pane);
    }

    private void disable(final ToggleSplitPane splitPane, final double weight, final double location) {
        if (splitPane.isResizable() && splitPane.isShowing()) {
            splitPane.savePosition();
        }
        splitPane.setResizeWeight(weight);
        splitPane.forceSetDividerLocation(location);
        splitPane.setResizable(false, location);
        splitPane.setEnabled(false);
    }

    /**
     * Returns whether the corresponding panel is currently enabled/visible.
     *
     * @param a the position of the panel.
     * @return true if enabled.
     */
    @Override
    public boolean isEnabled(final Alignment a) {
        if (a == Alignment.CENTER) {
            return false;
        } else {
            return enabled[a.getIndex()];
        }
    }

    @Override
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
    @Override
    public boolean[] getStatus() {
        return enabled;
    }

    @Override
    public PopupContainer getContainer(final Alignment alignment) {
        PopupContainer popupComponent = switch (alignment) {
            case NORTH -> (PopupContainer) topSplitter.getLeftComponent();
            case NORTH_EAST -> (PopupContainer) topSplitter.getRightComponent();
            case EAST -> (PopupContainer) rightSplitter.getTopComponent();
            case SOUTH_EAST -> (PopupContainer) rightSplitter.getBottomComponent();
            case SOUTH -> (PopupContainer) bottomSplitter.getRightComponent();
            case SOUTH_WEST -> (PopupContainer) bottomSplitter.getLeftComponent();
            case WEST -> (PopupContainer) leftSplitter.getBottomComponent();
            case NORTH_WEST -> (PopupContainer) leftSplitter.getTopComponent();
            default -> throw new IllegalArgumentException("CENTER is not supported");
        };
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

    protected record LayoutProportions(double splitRestore, double splitterPeerDisable, double splitDisable,
            double splitterDisable) {
    }

    protected record LayoutWeights(double splitEnable, double splitterDisable, double splitDisable,
            double splitterPeerDisable) {
    }

    protected static class TabFrameSplitPane extends ToggleSplitPane {
        protected TabFrameSplitPane(final String name) {
            super(name);
        }
    }
}
