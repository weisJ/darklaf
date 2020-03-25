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
package com.github.weisj.darklaf.ui.optionpane;

import sun.swing.DefaultLookup;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicOptionPaneUI;
import java.awt.*;

public class DarkOptionPaneUI extends BasicOptionPaneUI {


    public static ComponentUI createUI(final JComponent x) {
        return new DarkOptionPaneUI();
    }

    @Override
    protected Container createButtonArea() {
        JPanel bottom = new JPanel();
        Border border = (Border) DefaultLookup.get(optionPane, this, "OptionPane.buttonAreaBorder");
        bottom.setName("OptionPane.buttonArea");
        if (border != null) {
            bottom.setBorder(border);
        }
        bottom.setLayout(new DarkButtonAreaLayout(
            DefaultLookup.getBoolean(optionPane, this, "OptionPane.sameSizeButtons", false),
            DefaultLookup.getInt(optionPane, this, "OptionPane.buttonPadding", 6),
            DefaultLookup.getInt(optionPane, this, "OptionPane.buttonOrientation", SwingConstants.CENTER),
            DefaultLookup.getBoolean(optionPane, this, "OptionPane.isYesLast", false)));
        addButtonComponents(bottom, getButtons(), getInitialValueIndex());
        return bottom;
    }

    protected boolean getSizeButtonsToSameWidth() {
        return UIManager.getBoolean("OptionPane.sameSizeButtons");
    }

    /**
     * <code>ButtonAreaLayout</code> behaves in a similar manner to
     * <code>FlowLayout</code>. It lays out all components from left to
     * right. If <code>syncAllWidths</code> is true, the widths of each component will be set to the largest preferred
     * size width.
     * <p>
     * This class should be treated as a &quot;protected&quot; inner class. Instantiate it only within subclasses of
     * {@code BasicOptionPaneUI}.
     */
    public static class DarkButtonAreaLayout extends ButtonAreaLayout {
        protected boolean syncAllWidthOverwrite = false;
        private int orientation;
        private boolean reverseButtons;
        /**
         * Indicates whether or not centersChildren should be used vs the orientation. This is done for backward
         * compatibility for subclassers.
         */
        private boolean useOrientation;

        public DarkButtonAreaLayout(final boolean syncAllSizes, final int padding, final int orientation,
                                    final boolean reverseButtons) {
            this(syncAllSizes, padding);
            useOrientation = true;
            this.orientation = orientation;
            this.reverseButtons = reverseButtons;
        }

        /**
         * Constructs a new instance of {@code ButtonAreaLayout}.
         *
         * @param syncAllWidths if the width of children should be synchronized
         * @param padding       the padding value
         */
        public DarkButtonAreaLayout(final boolean syncAllWidths, final int padding) {
            super(syncAllWidths, padding);
            this.syncAllWidths = syncAllWidths;
            this.padding = padding;
            centersChildren = true;
            useOrientation = false;
        }

        /**
         * Sets if the width of children should be synchronized.
         *
         * @param newValue if the width of children should be synchronized
         */
        public void setSyncAllWidths(final boolean newValue) {
            syncAllWidths = newValue;
        }

        /**
         * Returns if the width of children should be synchronized.
         *
         * @return if the width of children should be synchronized
         */
        public boolean getSyncAllWidths() {
            return syncAllWidths && !syncAllWidthOverwrite;
        }

        /**
         * Sets the padding value.
         *
         * @param newPadding the new padding
         */
        public void setPadding(final int newPadding) {
            this.padding = newPadding;
        }

        /**
         * Returns the padding.
         *
         * @return the padding
         */
        public int getPadding() {
            return padding;
        }

        /**
         * Sets whether or not center children should be used.
         *
         * @param newValue a new value
         */
        public void setCentersChildren(final boolean newValue) {
            centersChildren = newValue;
            useOrientation = false;
        }

        /**
         * Returns whether or not center children should be used.
         *
         * @return whether or not center children should be used
         */
        public boolean getCentersChildren() {
            return centersChildren;
        }

        private int getOrientation(final Container container) {
            if (!useOrientation) {
                return SwingConstants.CENTER;
            }
            if (container.getComponentOrientation().isLeftToRight()) {
                return orientation;
            }
            switch (orientation) {
                case SwingConstants.LEFT:
                    return SwingConstants.RIGHT;
                case SwingConstants.RIGHT:
                    return SwingConstants.LEFT;
                case SwingConstants.CENTER:
                    return SwingConstants.CENTER;
            }
            return SwingConstants.LEFT;
        }

        public void addLayoutComponent(final String string, final Component comp) {
        }

        public void layoutContainer(final Container container) {
            Component[] children = container.getComponents();

            if (children != null && children.length > 0) {
                int numChildren = children.length;
                Insets insets = container.getInsets();
                int maxWidth = 0;
                int maxHeight = 0;
                int totalButtonWidth = 0;
                int x = 0;
                int xOffset = 0;
                boolean ltr = container.getComponentOrientation().isLeftToRight();
                boolean reverse = (ltr) == reverseButtons;

                for (Component child : children) {
                    Dimension pref = child.getPreferredSize();
                    maxWidth = Math.max(maxWidth, pref.width);
                    maxHeight = Math.max(maxHeight, pref.height);
                    totalButtonWidth += pref.width;
                }
                if (getSyncAllWidths()) {
                    int totalButtonWidthTmp = maxWidth * numChildren;
                    totalButtonWidthTmp += (numChildren - 1) * padding;

                    syncAllWidthOverwrite = totalButtonWidthTmp > container.getWidth();
                    if (!syncAllWidthOverwrite) {
                        totalButtonWidth = totalButtonWidthTmp;
                    }
                } else {
                    totalButtonWidth += (numChildren - 1) * padding;
                }


                switch (getOrientation(container)) {
                    case SwingConstants.LEFT:
                        x = insets.left;
                        break;
                    case SwingConstants.RIGHT:
                        x = container.getWidth() - insets.right - totalButtonWidth;
                        break;
                    case SwingConstants.CENTER:
                        if (getCentersChildren() || numChildren < 2) {
                            x = (container.getWidth() - totalButtonWidth) / 2;
                        } else {
                            x = insets.left;
                            if (getSyncAllWidths()) {
                                xOffset = (container.getWidth() - insets.left -
                                           insets.right - totalButtonWidth) /
                                          (numChildren - 1) + maxWidth;
                            } else {
                                xOffset = (container.getWidth() - insets.left -
                                           insets.right - totalButtonWidth) /
                                          (numChildren - 1);
                            }
                        }
                        break;
                }

                for (int counter = 0; counter < numChildren; counter++) {
                    int index = (reverse) ? numChildren - counter - 1 :
                                counter;
                    Dimension pref = children[index].getPreferredSize();

                    if (getSyncAllWidths()) {
                        children[index].setBounds(x, insets.top, maxWidth, maxHeight);
                    } else {
                        children[index].setBounds(x, insets.top, pref.width, pref.height);
                    }
                    if (xOffset != 0) {
                        x += xOffset;
                    } else {
                        x += children[index].getWidth() + padding;
                    }
                }
            }
            syncAllWidthOverwrite = false;
        }

        public Dimension minimumLayoutSize(final Container c) {
            if (c != null) {
                Component[] children = c.getComponents();

                if (children != null && children.length > 0) {
                    Dimension aSize;
                    int numChildren = children.length;
                    int height = 0;
                    Insets cInsets = c.getInsets();
                    int extraHeight = cInsets.top + cInsets.bottom;
                    int extraWidth = cInsets.left + cInsets.right;

                    if (getSyncAllWidths()) {
                        int maxWidth = 0;

                        for (Component child : children) {
                            aSize = child.getPreferredSize();
                            height = Math.max(height, aSize.height);
                            maxWidth = Math.max(maxWidth, aSize.width);
                        }
                        return new Dimension(extraWidth + (maxWidth * numChildren) +
                                             (numChildren - 1) * padding,
                                             extraHeight + height);
                    } else {
                        int totalWidth = 0;

                        for (Component child : children) {
                            aSize = child.getPreferredSize();
                            height = Math.max(height, aSize.height);
                            totalWidth += aSize.width;
                        }
                        totalWidth += ((numChildren - 1) * padding);
                        return new Dimension(extraWidth + totalWidth, extraHeight + height);
                    }
                }
            }
            return new Dimension(0, 0);
        }

        public Dimension preferredLayoutSize(final Container c) {
            return minimumLayoutSize(c);
        }

        public void removeLayoutComponent(final Component c) {
        }
    }
}
