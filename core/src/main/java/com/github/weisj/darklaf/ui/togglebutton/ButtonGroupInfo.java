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
package com.github.weisj.darklaf.ui.togglebutton;

import java.awt.*;
import java.util.Enumeration;
import java.util.HashSet;

import javax.swing.*;

public class ButtonGroupInfo {
    protected final AbstractButton activeButton;

    protected AbstractButton firstButton = null;
    protected AbstractButton lastButton = null;

    protected AbstractButton previousButton = null;
    protected AbstractButton nextButton = null;

    protected final HashSet<AbstractButton> buttonsInGroup;
    protected boolean sourceFound = false;

    public ButtonGroupInfo(final AbstractButton btn) {
        activeButton = btn;
        buttonsInGroup = new HashSet<>();
    }

    public static boolean isValidButton(final Object obj) {
        return obj instanceof AbstractButton && ((AbstractButton) obj).isEnabled()
                && ((AbstractButton) obj).isVisible();
    }

    protected boolean containsInGroup(final AbstractButton button) {
        return buttonsInGroup.contains(button);
    }

    protected Component getFocusTransferBaseComponent(final boolean next) {
        return firstButton;
    }

    protected boolean getButtonGroupInfo() {
        if (activeButton == null) {
            return false;
        }

        buttonsInGroup.clear();

        // Get the button model from the source.
        ButtonModel model = activeButton.getModel();
        if (!(model instanceof DefaultButtonModel bm)) {
            return false;
        }

        // If the button model is DefaultButtonModel, and use it, otherwise return.

        // get the ButtonGroup of the button from the button model
        ButtonGroup group = bm.getGroup();
        if (group == null) {
            return false;
        }

        // Get all the buttons in the group
        Enumeration<AbstractButton> e = group.getElements();
        if (e == null) {
            return false;
        }

        while (e.hasMoreElements()) {
            AbstractButton curElement = e.nextElement();
            if (!isValidButton(curElement)) {
                continue;
            }

            buttonsInGroup.add(curElement);

            // If firstBtn is not set yet, curElement is that first button
            if (firstButton == null) {
                firstButton = curElement;
            }

            if (activeButton == curElement) {
                sourceFound = true;
            } else if (!sourceFound) {
                // The source has not been yet found and the current element
                // is the last previousBtn
                previousButton = curElement;
            } else if (nextButton == null) {
                // The source has been found and the current element
                // is the next valid button of the list
                nextButton = curElement;
            }

            // Set new last "valid" AbstractButton of the list
            lastButton = curElement;
        }

        return true;
    }

    /**
     * Find the new radio button that focus needs to be moved to in the group, select the button
     *
     * @param next indicate if it's arrow up/left or down/right
     */
    protected void selectNewButton(final boolean next) {
        if (!getButtonGroupInfo()) {
            return;
        }

        if (sourceFound) {
            AbstractButton newSelectedButton;
            if (next) {
                // Select Next button. Cycle to the first button if the source.
                // button is the last of the group.
                newSelectedButton = (null == nextButton) ? firstButton : nextButton;
            } else {
                // Select previous button. Cycle to the last button if the source.
                // button is the first button of the group.
                newSelectedButton = (null == previousButton) ? lastButton : previousButton;
            }
            if (newSelectedButton != null && (newSelectedButton != activeButton)) {
                newSelectedButton.requestFocusInWindow();
            }
        }
    }

    /**
     * Find the button group the passed in AbstractButton belongs to, and move focus to next component
     * of the last button in the group or previous component of first button
     *
     * @param next indicate if jump to next component or previous
     */
    protected void jumpToNextComponent(final boolean next) {
        if (!getButtonGroupInfo()) {
            // In case the button does not belong to any group, it needs
            // to be treated as a component
            if (activeButton != null) {
                lastButton = activeButton;
                firstButton = activeButton;
            } else {
                return;
            }
        }

        // If next component in the parent window is not in
        // the button group, current active button will be
        // base, otherwise, the base will be first or last
        // button in the button group
        Component focusBase = getFocusTransferBaseComponent(next);
        if (focusBase != null) {
            if (next) {
                KeyboardFocusManager.getCurrentKeyboardFocusManager().focusNextComponent(focusBase);
            } else {
                KeyboardFocusManager.getCurrentKeyboardFocusManager().focusPreviousComponent(focusBase);
            }
        }
    }
}
