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
package com.github.weisj.darklaf.components.tabframe;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.basic.BasicSplitPaneDivider;
import javax.swing.plaf.basic.BasicSplitPaneUI;

import com.github.weisj.darklaf.ui.splitpane.DarkSplitPaneUI;

public class ToggleSplitPane extends JSplitPane {

    private boolean resizable = true;
    private boolean lastEnabled = true;
    private double restorePercentage;
    private double lockedPosition;
    private boolean isInLayout = false;
    private boolean enabled = true;

    public ToggleSplitPane() {
        this(null);
    }

    public ToggleSplitPane(final String name) {
        setName(name);
        putClientProperty(DarkSplitPaneUI.KEY_STYLE, DarkSplitPaneUI.STYLE_INVISIBLE);
    }

    public boolean isResizable() {
        return resizable;
    }

    /**
     * Set if the split pane should be able to resize. Locking the position will ignore the resize
     * weights and distribute the content according to the current relative position.
     *
     * @param resizable true if it should resize.
     */
    public void setResizable(final boolean resizable) {
        setResizable(resizable, getRelativeDividerLocation());
    }

    /**
     * Set if the split pane should be able to resize. Locking the position will ignore the resize
     * weights and distribute the content according to the locked relative position.
     *
     * @param resizable true if it should resize.
     * @param lockedPosition the relative position to lock the split pane in.
     */
    public void setResizable(final boolean resizable, final double lockedPosition) {
        this.resizable = resizable;
        if (!resizable) {
            lastEnabled = isEnabled();
            setLockedPosition(lockedPosition);
            setEnabled(false);
            getDivider().setEnabled(false);
            setComponentZOrder(getDivider(), getComponentCount() - 1);
        } else {
            setEnabled(lastEnabled);
            getDivider().setEnabled(lastEnabled);
            setComponentZOrder(getDivider(), 0);
        }
    }

    protected void setLockedPosition(final double lockedPosition) {
        this.lockedPosition = lockedPosition;
    }

    protected BasicSplitPaneDivider getDivider() {
        return ((BasicSplitPaneUI) getUI()).getDivider();
    }

    @Override
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
        ((BasicSplitPaneUI) getUI()).getDivider().setEnabled(enabled);
    }

    @Override
    public void updateUI() {
        super.updateUI();
        setEnabled(enabled);
        setResizable(isResizable());
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

    public double getRestorePosition() {
        return restorePercentage;
    }

    public double getLockedPosition() {
        return lockedPosition;
    }

    public void forceSetDividerLocation(final int location) {
        super.setDividerLocation(location);
    }

    public void forceSetDividerLocation(final double proportionalLocation) {
        super.setDividerLocation(getLocationForRelativePosition(proportionalLocation));
    }

    private int getLocationForRelativePosition(final double relativePosition) {
        if (relativePosition < 0.0 || relativePosition > 1.0) {
            throw new IllegalArgumentException("proportional location must " + "be between 0.0 and 1.0.");
        }
        if (getOrientation() == VERTICAL_SPLIT) {
            return (int) ((double) (getHeight()) * relativePosition);
        } else {
            return (int) ((double) (getWidth()) * relativePosition);
        }
    }

    @Override
    public int getLastDividerLocation() {
        if (isResizable()) {
            return super.getLastDividerLocation();
        } else {
            return getLocationForRelativePosition(lockedPosition);
        }
    }

    @Override
    public int getDividerLocation() {
        if (isResizable()) {
            return super.getDividerLocation();
        } else {
            return getLocationForRelativePosition(lockedPosition);
        }
    }

    @Override
    public void setDividerLocation(final int location) {
        if (isInLayout) return;
        isInLayout = true;
        if (isResizable()) {
            super.setDividerLocation(location);
        } else {
            forceSetDividerLocation(restorePercentage);
            doLayout();
        }
        isInLayout = false;
    }

    @Override
    public int getMaximumDividerLocation() {
        int max = getOrientation() == HORIZONTAL_SPLIT ? getWidth() : getHeight();
        return Math.max(max, getMinimumDividerLocation());
    }

    @Override
    public int getMinimumDividerLocation() {
        Component comp = getRightComponent();
        return comp == null ? 0
                : getOrientation() == HORIZONTAL_SPLIT ? comp.getMinimumSize().width : comp.getMinimumSize().height;
    }
}
