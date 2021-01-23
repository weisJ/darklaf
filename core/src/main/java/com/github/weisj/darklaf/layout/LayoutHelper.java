/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
 *
 */
package com.github.weisj.darklaf.layout;

import java.awt.*;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Supplier;

import javax.swing.*;
import javax.swing.border.Border;

import com.github.weisj.darklaf.components.HoveringPanel;
import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.components.border.DarkBorders;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.DarkUIUtil;

public final class LayoutHelper {

    private LayoutHelper() {
        throw new IllegalArgumentException("Utility class");
    }

    public static int getDefaultSpacing() {
        return 5;
    }


    public static Border createEmptyBorder(final Insets ins) {
        return BorderFactory.createEmptyBorder(ins.top, ins.left, ins.bottom, ins.right);
    }

    public static Border createEmptyContainerBorder() {
        int pad = getDefaultSpacing();
        return BorderFactory.createEmptyBorder(pad, pad, pad, pad);
    }

    public static Insets createEmptyContainerInsets() {
        int pad = getDefaultSpacing();
        return new Insets(pad, pad, pad, pad);
    }

    public static JComponent createTwoColumnPanel(final JComponent[] left, final JComponent[] right) {
        return createTwoColumnPanel(Arrays.asList(left), Arrays.asList(right));
    }

    public static JComponent createTwoColumnPanel(final JComponent[] left, final JComponent[] right,
            final GroupLayout.Alignment leftColumn, final GroupLayout.Alignment rightColumn) {
        return createTwoColumnPanel(Arrays.asList(left), Arrays.asList(right), leftColumn, rightColumn);
    }

    public static JComponent createTwoColumnPanel(final List<JComponent> left, final List<JComponent> right) {
        return createTwoColumnPanel(left, right, GroupLayout.Alignment.TRAILING, GroupLayout.Alignment.LEADING);
    }

    public static JComponent createTwoColumnPanel(final List<JComponent> left, final List<JComponent> right,
            final GroupLayout.Alignment leftColumn, final GroupLayout.Alignment rightColumn) {
        if (left.size() != right.size()) {
            String s = left.size() + " labels supplied for " + right.size() + " fields!";
            throw new IllegalArgumentException(s);
        }
        JComponent panel = new JPanel();
        GroupLayout layout = new GroupLayout(panel);
        panel.setLayout(layout);
        layout.setAutoCreateGaps(true);
        layout.setAutoCreateContainerGaps(false);

        // Create a sequential group for the horizontal axis.
        GroupLayout.SequentialGroup horizontalGroup = layout.createSequentialGroup();

        GroupLayout.Group verticalLabelGroup = layout.createParallelGroup(leftColumn);
        horizontalGroup.addGroup(verticalLabelGroup);

        GroupLayout.Group verticalComponentGroup = layout.createParallelGroup(rightColumn);
        horizontalGroup.addGroup(verticalComponentGroup);
        layout.setHorizontalGroup(horizontalGroup);

        // Create a sequential group for the vertical axis.
        GroupLayout.SequentialGroup verticalGroup = layout.createSequentialGroup();
        layout.setVerticalGroup(verticalGroup);

        int p = GroupLayout.PREFERRED_SIZE;
        for (int i = 0; i < left.size(); i++) {
            JComponent lComp = left.get(i);
            JComponent rComp = right.get(i);

            panel.add(lComp);
            panel.add(rComp);

            verticalLabelGroup.addComponent(lComp, p, p, p);
            verticalComponentGroup.addComponent(rComp, p, p, p);

            verticalGroup.addGroup(layout.createParallelGroup(GroupLayout.Alignment.CENTER)
                    .addComponent(lComp, p, p, p)
                    .addComponent(rComp, p, p, p));
        }
        return panel;
    }

    public static JComponent createTitledColumn(final List<String> titles, final List<JComponent> components,
            final int indent) {
        if (titles.size() != components.size()) {
            String s = titles.size() + " labels supplied for " + components.size() + " components!";
            throw new IllegalArgumentException(s);
        }
        JComponent panel = new JPanel();
        GroupLayout layout = new GroupLayout(panel);
        panel.setLayout(layout);
        layout.setAutoCreateGaps(true);
        layout.setAutoCreateContainerGaps(false);

        GroupLayout.ParallelGroup parallelGroup = layout.createParallelGroup();
        GroupLayout.SequentialGroup sequentialGroup = layout.createSequentialGroup();

        int p = GroupLayout.PREFERRED_SIZE;

        for (int i = 0; i < titles.size(); i++) {
            JLabel label = new JLabel(titles.get(i));
            JComponent c = components.get(i);
            sequentialGroup.addComponent(label, p, p, p);
            sequentialGroup.addComponent(c, p, p, p);
            parallelGroup.addComponent(label, p, p, p);
            GroupLayout.SequentialGroup sq = layout.createSequentialGroup();
            if (indent >= 0) {
                sq.addGap(indent);
            } else {
                sq.addPreferredGap(label, c, LayoutStyle.ComponentPlacement.INDENT);
            }
            sq.addComponent(c, p, p, p);
            parallelGroup.addGroup(sq);
        }

        layout.setHorizontalGroup(parallelGroup);
        layout.setVerticalGroup(sequentialGroup);
        return panel;
    }

    public static JComponent createPanelWithHoverOverlay(final JComponent content,
            final JComponent overlayContent) {
        return createPanelWithHoverOverlay(content, overlayContent, Alignment.NORTH_EAST, createEmptyContainerInsets());
    }

    public static JComponent createPanelWithHoverOverlay(final JComponent content,
            final JComponent overlayContent,
            final Alignment alignment, final Insets padding) {
        return createPanelWithHoverOverlay(content, overlayContent, (r, c) -> {
            Dimension prefSize = c.getPreferredSize();
            DarkUIUtil.applyInsets(r, padding);
            Point p = alignment.alignInside(prefSize, r);
            return new Rectangle(p, prefSize);
        });
    }

    public static JComponent createPanelWithHoverOverlay(final JComponent content, final JComponent overlayContent,
            final BiFunction<Rectangle, JComponent, Rectangle> layoutFunction) {
        HoveringPanel hoveringPanel = new HoveringPanel();
        hoveringPanel.add(overlayContent);
        return createPanelWithOverlay(content, hoveringPanel, (r, c) -> {
            hoveringPanel.setVisible(overlayContent.isVisible());
            return layoutFunction.apply(r, hoveringPanel);
        });
    }


    public static JComponent createPanelWithOverlay(final JComponent content, final JComponent overlayContent) {
        return createPanelWithOverlay(content, overlayContent, Alignment.NORTH_EAST, createEmptyContainerInsets());
    }

    public static JComponent createPanelWithOverlay(final JComponent content, final JComponent overlayContent,
            final Alignment alignment, final Insets padding) {
        return createPanelWithOverlay(content, overlayContent, (r, c) -> {
            Dimension prefSize = c.getPreferredSize();
            DarkUIUtil.applyInsets(r, padding);
            Point p = alignment.alignInside(prefSize, r);
            return new Rectangle(p, prefSize);
        });
    }

    public static JComponent createPanelWithOverlay(final JComponent content, final JComponent overlayContent,
            final BiFunction<Rectangle, JComponent, Rectangle> layoutFunction) {
        return new JLayeredPane() {

            {
                add(content, JLayeredPane.DEFAULT_LAYER);
                add(overlayContent, Integer.valueOf(JLayeredPane.MODAL_LAYER - 1));
            }

            @Override
            public void doLayout() {
                super.doLayout();
                Rectangle bounds = new Rectangle(0, 0, getWidth(), getHeight());
                content.setBounds(bounds);
                content.doLayout();
                overlayContent.setBounds(layoutFunction.apply(bounds, overlayContent));
                overlayContent.doLayout();
            }

            @Override
            public Dimension getPreferredSize() {
                Dimension dim = super.getPreferredSize();
                Dimension contentSize = content.getPreferredSize();
                dim.width = Math.max(dim.width, contentSize.width);
                dim.height = Math.max(dim.height, contentSize.height);
                if (overlayContent.isVisible()) {
                    Rectangle layoutRect = layoutFunction.apply(new Rectangle(dim), overlayContent);
                    dim.width = Math.max(dim.width, layoutRect.x + layoutRect.width);
                    dim.height = Math.max(dim.height, layoutRect.y + layoutRect.height);
                }
                return dim;
            }
        };
    }

    public static OverlayScrollPane createScrollPaneWithOverlay(final JComponent content,
            final JComponent overlayContent) {
        return createScrollPaneWithOverlay(content, overlayContent, Alignment.NORTH_EAST, createEmptyContainerInsets());
    }

    public static OverlayScrollPane createScrollPaneWithOverlay(final JComponent content,
            final JComponent overlayContent,
            final Alignment alignment, final Insets padding) {
        return createScrollPaneWithOverlay(content, overlayContent, (r, c) -> {
            Dimension prefSize = c.getPreferredSize();
            DarkUIUtil.applyInsets(r, padding);
            Point p = alignment.alignInside(prefSize, r);
            return new Rectangle(p, prefSize);
        });
    }

    public static OverlayScrollPane createScrollPaneWithHoverOverlay(final JComponent content,
            final JComponent overlayContent,
            final Alignment alignment, final Supplier<Insets> paddingSupplier) {
        HoveringPanel hoveringPanel = new HoveringPanel() {
            @Override
            public boolean isVisible() {
                return overlayContent.isVisible();
            }
        };
        hoveringPanel.add(overlayContent);
        return createScrollPaneWithOverlay(content, hoveringPanel, alignment, paddingSupplier);
    }

    public static OverlayScrollPane createScrollPaneWithOverlay(final JComponent content,
            final JComponent overlayContent,
            final Alignment alignment, final Supplier<Insets> paddingSupplier) {
        return createScrollPaneWithOverlay(content, overlayContent, (r, c) -> {
            Dimension prefSize = c.getPreferredSize();
            DarkUIUtil.applyInsets(r, paddingSupplier.get());
            Point p = alignment.alignInside(prefSize, r);
            return new Rectangle(p, prefSize);
        });
    }

    public static OverlayScrollPane createScrollPaneWithHoverOverlay(final JComponent content,
            final JComponent overlayContent,
            final BiFunction<Rectangle, JComponent, Rectangle> layoutFunction) {
        HoveringPanel hoveringPanel = new HoveringPanel();
        hoveringPanel.add(overlayContent);
        return createScrollPaneWithOverlay(content, hoveringPanel, (r, c) -> {
            hoveringPanel.setVisible(overlayContent.isVisible());
            return layoutFunction.apply(r, hoveringPanel);
        });
    }

    public static OverlayScrollPane createScrollPaneWithOverlay(final JComponent content,
            final JComponent overlayContent,
            final BiFunction<Rectangle, JComponent, Rectangle> layoutFunction) {
        return new OverlayScrollPane(content) {

            {
                add(overlayContent, Integer.valueOf(JLayeredPane.MODAL_LAYER - 1));
            }

            @Override
            public void doLayout() {
                super.doLayout();
                Component viewport = getScrollPane().getViewport();
                overlayContent.setBounds(
                        layoutFunction.apply(getLayoutRect(viewport.getWidth(), viewport.getHeight()), overlayContent));
                overlayContent.doLayout();
                overlayContent.setVisible(overlayContent.isVisible());
            }

            private Rectangle getLayoutRect(final int width, final int height) {
                Component viewport = getScrollPane().getViewport();
                Rectangle viewBounds = new Rectangle(0, 0, width, height);
                viewBounds = SwingUtilities.convertRectangle(viewport, viewBounds, this);
                JScrollBar vertBar = getVerticalScrollBar();
                JScrollBar horBar = getHorizontalScrollBar();
                if (vertBar != null && vertBar.isVisible()) {
                    viewBounds.width -= vertBar.getWidth();
                }
                if (horBar != null && horBar.isVisible()) {
                    viewBounds.height -= horBar.getHeight();
                }
                return viewBounds;
            }

            @Override
            public Dimension getPreferredSize() {
                Dimension dim = super.getPreferredSize();
                if (overlayContent.isVisible()) {
                    Rectangle layoutRect = layoutFunction.apply(getLayoutRect(dim.width, dim.height), overlayContent);
                    dim.width = Math.max(dim.width, layoutRect.x + layoutRect.width);
                    dim.height = Math.max(dim.height, layoutRect.y + layoutRect.height);
                }
                return dim;
            }
        };
    }

    public static Box createHorizontalBox(final int padding, final JComponent... comps) {
        Box box = Box.createHorizontalBox();
        if (comps != null) {
            for (int i = 0; i < comps.length - 1; i++) {
                box.add(comps[i]);
                box.add(Box.createHorizontalStrut(padding));
            }
            box.add(comps[comps.length - 1]);
        }
        return box;
    }

    public static Box createVerticalBox(final int padding, final JComponent... comps) {
        Box box = Box.createVerticalBox();
        if (comps != null) {
            for (int i = 0; i < comps.length - 1; i++) {
                box.add(comps[i]);
                box.add(Box.createVerticalStrut(padding));
            }
            box.add(comps[comps.length - 1]);
        }
        return box;
    }

    public static JComponent createMultiColumnPanel(final List<? extends JComponent> components) {
        JPanel panel = new JPanel();
        GroupLayout layout = new GroupLayout(panel);
        panel.setLayout(layout);
        layout.setAutoCreateGaps(false);
        layout.setAutoCreateContainerGaps(false);
        GroupLayout.SequentialGroup horizontal = layout.createSequentialGroup();
        GroupLayout.ParallelGroup vertical = layout.createParallelGroup();
        layout.setHorizontalGroup(horizontal);
        layout.setVerticalGroup(vertical);

        int p = GroupLayout.PREFERRED_SIZE;

        for (int i = 0; i < components.size(); i++) {
            JPanel holder = new JPanel(new BorderLayout());
            if (i > 0) holder.setBorder(DarkBorders.createLeftBorder());
            holder.add(components.get(i));
            horizontal.addComponent(holder);
            vertical.addComponent(holder);
        }
        return panel;
    }
}
