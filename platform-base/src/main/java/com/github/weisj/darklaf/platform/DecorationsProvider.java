/*
 * MIT License
 *
 * Copyright (c) 2019-2022 Jannis Weis
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
package com.github.weisj.darklaf.platform;

import java.awt.*;
import java.util.List;

import javax.swing.*;

public interface DecorationsProvider {

    /**
     * Create the custom title pane.
     *
     * @param rootPane The root pane to create the title pane for.
     * @param decorationStyle The style of the decorations.
     * @param window The target window.
     * @return the custom title pane.
     * @see JRootPane#getWindowDecorationStyle()
     */
    CustomTitlePane createTitlePane(final JRootPane rootPane, final int decorationStyle, final Window window);

    /**
     * Returns whether custom decorations are supported.
     *
     * @return true if decorations are supported.
     */
    boolean isCustomDecorationSupported();

    /** Initialize all necessary resources. */
    void initialize();

    /**
     * Returns the path to the decoration properties.
     *
     * @return path to load a resources from.
     */
    List<String> getPropertyResourcePaths();

    /** Initialize the window of a popup menu. */
    default void installPopupWindow(final Window window) {}

    /** Uninstall the window of a popup menu. */
    default void uninstallPopupWindow(final Window window) {}

    /**
     * Adjusts the content area.
     *
     * @param root the corresponding root pane.
     * @param rect the proposed content area.
     */
    default void adjustContentArea(final JRootPane root, final Rectangle rect) {}

    /**
     * Returns whether the system provides a title label for us.
     *
     * @return true of the system paints the title label.
     */
    default boolean supportsNativeTitleLabel() {
        return false;
    }

    /**
     * Compute the layout information for the title pane.
     *
     * @param customTitlePane the given title pane
     * @return information about the layout of the title pane
     */
    TitlePaneLayoutInfo titlePaneLayoutInfo(final CustomTitlePane customTitlePane);
}
