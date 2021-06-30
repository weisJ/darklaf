/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
package com.github.weisj.darklaf.components.loading;

import javax.swing.*;

import com.github.weisj.darklaf.components.RotatableIconAnimator;
import com.github.weisj.darklaf.properties.icons.EmptyIcon;
import com.github.weisj.darklaf.properties.icons.RotatableIcon;
import com.github.weisj.darklaf.properties.icons.TwoIcon;

/** Label that functions as an loading indicator. */
public class LoadingIndicator extends JLabel {

    private final RotatableIcon loadIcon;
    private final Icon pausedIcon;
    private final Icon emptyIcon;
    private final TwoIcon displayIcon;
    private final RotatableIconAnimator animator;
    private boolean running;

    public LoadingIndicator(final String text, final Icon icon, final int horizontalAlignment) {
        super(text, icon, horizontalAlignment);
        loadIcon = new RotatableIcon(UIManager.getIcon("LoadIndicator.stepWorkingIcon"));
        pausedIcon = UIManager.getIcon("LoadIndicator.stepPassiveIcon");
        emptyIcon = EmptyIcon.create(loadIcon);
        displayIcon = new TwoIcon(loadIcon, null);
        animator = new RotatableIconAnimator(8, loadIcon, this);
        displayIcon.setIconGap(getIconTextGap());
    }

    public LoadingIndicator(final String text, final int horizontalAlignment) {
        this(text, null, horizontalAlignment);
    }

    public LoadingIndicator(final String text) {
        this(text, null, LEADING);
    }

    public LoadingIndicator(final Icon image, final int horizontalAlignment) {
        this(null, image, horizontalAlignment);
    }

    public LoadingIndicator(final Icon image) {
        this(null, image, CENTER);
    }

    public LoadingIndicator() {
        this("", null, LEADING);
    }

    /**
     * Sets whether the icon should be animated.
     *
     * @param running true if animated.
     */
    public void setRunning(final boolean running) {
        this.running = running;
        setAnimatorState(running);
    }

    private void setAnimatorState(final boolean running) {
        if (running == animator.isRunning()) return;
        if (running) {
            animator.resume();
        } else {
            animator.suspend();
        }
        repaint();
    }

    @Override
    public void setEnabled(final boolean enabled) {
        super.setEnabled(enabled);
        setAnimatorState(isRunning());
    }

    /**
     * Returns whether the loading icon is animated and visible.
     *
     * @see #setRunning(boolean)
     * @return true if animated and visible.
     */
    public boolean isRunning() {
        return running && isEnabled();
    }

    @Override
    public void setIconTextGap(final int iconTextGap) {
        super.setIconTextGap(iconTextGap);
        displayIcon.setIconGap(getIconTextGap());
    }

    @Override
    public Icon getIcon() {
        displayIcon.setLeftIcon(running ? loadIcon : pausedIcon);
        displayIcon.setRightIcon(super.getIcon());
        return displayIcon;
    }

    @Override
    public Icon getDisabledIcon() {
        displayIcon.setLeftIcon(emptyIcon);
        displayIcon.setRightIcon(super.getDisabledIcon());
        return displayIcon;
    }
}
