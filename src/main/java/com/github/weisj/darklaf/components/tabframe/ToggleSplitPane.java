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

import javax.swing.*;
import javax.swing.plaf.basic.BasicSplitPaneDivider;
import javax.swing.plaf.basic.BasicSplitPaneUI;
import java.awt.*;

public class ToggleSplitPane extends JSplitPane {

    private int disabledPos = 0;
    private int disabledMax = -1;
    private boolean resizable = true;
    private boolean lastEnabled = true;
    private double restorePercentage;


    public ToggleSplitPane() {
        this(null);
    }

    public ToggleSplitPane(final String name) {
        setName(name);
        putClientProperty("JSplitPane.style", "invisible");
    }


    public boolean isResizable() {
        return resizable;
    }

    /**
     * Set if the split pane should be able to resize.
     *
     * @param resizable true if it should resize.
     */
    public void setResizable(final boolean resizable) {
        this.resizable = resizable;
        if (!resizable) {
            lastEnabled = isEnabled();
            setEnabled(false);
            disabledPos = super.getDividerLocation();
            disabledMax = getMaximumDividerLocation();
            getDivider().setEnabled(false);
            setComponentZOrder(getDivider(), getComponentCount() - 1);
        } else {
            setEnabled(lastEnabled);
            getDivider().setEnabled(lastEnabled);
            setComponentZOrder(getDivider(), 0);
        }
    }

    protected BasicSplitPaneDivider getDivider() {
        return ((BasicSplitPaneUI) getUI()).getDivider();
    }

    @Override
    public void setEnabled(final boolean enabled) {
        ((BasicSplitPaneUI) getUI()).getDivider().setEnabled(enabled);
    }

    public void savePosition() {
        savePosition(getRelativeDividerLocation());
    }

    public void savePosition(final double position) {
        restorePercentage = Math.max(0, Math.min(1.0, position));
    }

    public double getRelativeDividerLocation() {
        if (getOrientation() == HORIZONTAL_SPLIT) {
            return getDividerLocation() / (double) getWidth();
        } else {
            return getDividerLocation() / (double) getHeight();
        }
    }

    public void restorePosition() {
        setDividerLocation(restorePercentage);
        doLayout();
    }

    public void forceSetDividerLocation(final int location) {
        super.setDividerLocation(location);
    }

    public void forceSetDividerLocation(final double proportionalLocation) {
        if (proportionalLocation < 0.0 ||
                proportionalLocation > 1.0) {
            throw new IllegalArgumentException("proportional location must "
                                                       + "be between 0.0 and 1.0.");
        }
        if (getOrientation() == VERTICAL_SPLIT) {
            super.setDividerLocation((int) ((double) (getHeight()) * proportionalLocation));
        } else {
            super.setDividerLocation((int) ((double) (getWidth()) * proportionalLocation));
        }
    }

    @Override
    public int getLastDividerLocation() {
        if (resizable) {
            return super.getLastDividerLocation();
        } else {
            return disabledMax == disabledPos ? getMaximumDividerLocation() : disabledPos;
        }
    }

    @Override
    public int getDividerLocation() {
        if (resizable) {
            return super.getDividerLocation();
        } else {
            return disabledMax == disabledPos ? getMaximumDividerLocation() : disabledPos;
        }
    }


    @Override
    public void setDividerLocation(final int location) {
        if (resizable) {
            super.setDividerLocation(location);
        } else if (disabledPos == disabledMax) {
            super.setDividerLocation(getMaximumDividerLocation());
            doLayout();
        } else if (disabledPos == 0) {
            super.setDividerLocation(0);
            doLayout();
        }
    }


    @Override
    public int getMaximumDividerLocation() {
        int max = getOrientation() == HORIZONTAL_SPLIT ? getWidth() : getHeight();
        return Math.max(max, getMinimumDividerLocation());
    }

    @Override
    public int getMinimumDividerLocation() {
        Component comp = getRightComponent();
        return comp == null ? 0 : getOrientation() == HORIZONTAL_SPLIT
                                  ? comp.getMinimumSize().width
                                  : comp.getMinimumSize().height;
    }

}
