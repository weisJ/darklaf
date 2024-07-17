/*
 * MIT License
 *
 * Copyright (c) 2020-2024 Jannis Weis
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
package com.github.weisj.darklaf.ui.scrollpane;

import java.awt.*;
import java.awt.event.MouseWheelEvent;

import javax.swing.*;

import com.github.weisj.darklaf.util.PropertyUtil;

public class ScrollBarUtil implements ScrollBarConstants {

    public static JScrollPane getScrollPane(final JScrollBar scrollBar) {
        Component parent = scrollBar.getParent();
        if (parent instanceof JScrollPane) return (JScrollPane) parent;
        return PropertyUtil.getObject(scrollBar, KEY_SCROLL_PANE_PARENT, JScrollPane.class);
    }

    @SuppressWarnings("MagicConstant")
    public static void doScroll(final JScrollBar toScroll, final JViewport vp, final MouseWheelEvent e,
            final boolean leftToRight) {
        int direction = e.getWheelRotation() < 0 ? -1 : 1;
        int orientation = toScroll.getOrientation();
        if (!leftToRight && orientation == JScrollBar.HORIZONTAL) {
            direction *= -1;
        }

        if (e.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL) {
            int units = Math.abs(e.getUnitsToScroll());

            boolean limitScroll = Math.abs(e.getWheelRotation()) == 1;

            Component comp = vp == null ? null : vp.getView();
            if (comp instanceof Scrollable scrollComp
                    && PropertyUtil.getBooleanProperty(toScroll, KEY_FAST_WHEEL_SCROLLING)) {
                Rectangle viewRect = vp.getViewRect();
                int startingX = viewRect.x;
                int scrollMin = toScroll.getMinimum();
                int scrollMax = toScroll.getMaximum() - toScroll.getModel().getExtent();

                if (limitScroll) {
                    int blockIncr = scrollComp.getScrollableBlockIncrement(viewRect, orientation, direction);
                    if (direction < 0) {
                        scrollMin = Math.max(scrollMin, toScroll.getValue() - blockIncr);
                    } else {
                        scrollMax = Math.min(scrollMax, toScroll.getValue() + blockIncr);
                    }
                }

                for (int i = 0; i < units; i++) {
                    int unitIncr = scrollComp.getScrollableUnitIncrement(viewRect, orientation, direction);
                    // Modify the visible rect for the next unit, and
                    // check to see if we're at the end already.
                    if (orientation == SwingConstants.VERTICAL) {
                        if (direction < 0) {
                            viewRect.y -= unitIncr;
                            if (viewRect.y <= scrollMin) {
                                viewRect.y = scrollMin;
                                break;
                            }
                        } else { // (direction > 0
                            viewRect.y += unitIncr;
                            if (viewRect.y >= scrollMax) {
                                viewRect.y = scrollMax;
                                break;
                            }
                        }
                    } else {
                        // Scroll left
                        if ((leftToRight && direction < 0) || (!leftToRight && direction > 0)) {
                            viewRect.x -= unitIncr;
                            if (leftToRight) {
                                if (viewRect.x < scrollMin) {
                                    viewRect.x = scrollMin;
                                    break;
                                }
                            }
                        }
                        // Scroll right
                        else {
                            viewRect.x += unitIncr;
                            if (leftToRight) {
                                if (viewRect.x > scrollMax) {
                                    viewRect.x = scrollMax;
                                    break;
                                }
                            }
                        }
                    }
                }
                // Set the final view position on the ScrollBar
                if (orientation == SwingConstants.VERTICAL) {
                    toScroll.setValue(viewRect.y);
                } else {
                    if (leftToRight) {
                        toScroll.setValue(viewRect.x);
                    } else {
                        // rightToLeft scrollbars are oriented with
                        // minValue on the right and maxValue on the
                        // left.
                        int newPos = toScroll.getValue() - (viewRect.x - startingX);
                        if (newPos < scrollMin) {
                            newPos = scrollMin;
                        } else if (newPos > scrollMax) {
                            newPos = scrollMax;
                        }
                        toScroll.setValue(newPos);
                    }
                }
            } else {
                // Viewport's view is not a Scrollable, or fast wheel
                // scrolling is not enabled.
                scrollByUnits(toScroll, direction, units, limitScroll);
            }
        } else if (e.getScrollType() == MouseWheelEvent.WHEEL_BLOCK_SCROLL) {
            scrollByBlock(toScroll, direction);
        }
    }

    static void scrollByUnits(final JScrollBar scrollbar, final int direction, final int units,
            final boolean limitToBlock) {
        // This method is called from BasicScrollPaneUI to implement wheel
        // scrolling, as well as from scrollByUnit().
        int delta;
        int limit = -1;

        if (limitToBlock) {
            if (direction < 0) {
                limit = scrollbar.getValue() - scrollbar.getBlockIncrement(direction);
            } else {
                limit = scrollbar.getValue() + scrollbar.getBlockIncrement(direction);
            }
        }

        for (int i = 0; i < units; i++) {
            if (direction > 0) {
                delta = scrollbar.getUnitIncrement(direction);
            } else {
                delta = -scrollbar.getUnitIncrement(direction);
            }

            int oldValue = scrollbar.getValue();
            int newValue = oldValue + delta;

            // Check for overflow.
            if (delta > 0 && newValue < oldValue) {
                newValue = scrollbar.getMaximum();
            } else if (delta < 0 && newValue > oldValue) {
                newValue = scrollbar.getMinimum();
            }
            if (oldValue == newValue) {
                break;
            }

            if (limitToBlock && i > 0) {
                assert limit != -1;
                if ((direction < 0 && newValue < limit) || (direction > 0 && newValue > limit)) {
                    break;
                }
            }
            scrollbar.setValue(newValue);
        }
    }

    static void scrollByBlock(final JScrollBar scrollbar, final int direction) {
        // This method is called from BasicScrollPaneUI to implement wheel
        // scrolling, and also from scrollByBlock().
        int oldValue = scrollbar.getValue();
        int blockIncrement = scrollbar.getBlockIncrement(direction);
        int delta = blockIncrement * ((direction > 0) ? +1 : -1);
        int newValue = oldValue + delta;

        // Check for overflow.
        if (delta > 0 && newValue < oldValue) {
            newValue = scrollbar.getMaximum();
        } else if (delta < 0 && newValue > oldValue) {
            newValue = scrollbar.getMinimum();
        }

        scrollbar.setValue(newValue);
    }
}
