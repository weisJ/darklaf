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
package com.github.weisj.darklaf.task;

import com.github.weisj.darklaf.theme.Theme;

import javax.swing.*;

public class IdeaDefaultsInitTask implements DefaultsInitTask {
    @Override
    public void run(final Theme currentTheme, final UIDefaults defaults) {
        initIdeaDefaults(defaults);
    }

    @SuppressWarnings({"HardCodedStringLiteral"})
    private void initIdeaDefaults(final UIDefaults defaults) {
        defaults.put("Table.ancestorInputMap", new UIDefaults.LazyInputMap(
            new Object[]{
                "ctrl C", "copy",
                "meta C", "copy",
                "ctrl V", "paste",
                "meta V", "paste",
                "ctrl X", "cut",
                "meta X", "cut",
                "COPY", "copy",
                "PASTE", "paste",
                "CUT", "cut",
                "control INSERT", "copy",
                "shift INSERT", "paste",
                "shift DELETE", "cut",
                "RIGHT", "selectNextColumn",
                "KP_RIGHT", "selectNextColumn",
                "LEFT", "selectPreviousColumn",
                "KP_LEFT", "selectPreviousColumn",
                "DOWN", "selectNextRow",
                "KP_DOWN", "selectNextRow",
                "UP", "selectPreviousRow",
                "KP_UP", "selectPreviousRow",
                "shift RIGHT", "selectNextColumnExtendSelection",
                "shift KP_RIGHT", "selectNextColumnExtendSelection",
                "shift LEFT", "selectPreviousColumnExtendSelection",
                "shift KP_LEFT", "selectPreviousColumnExtendSelection",
                "shift DOWN", "selectNextRowExtendSelection",
                "shift KP_DOWN", "selectNextRowExtendSelection",
                "shift UP", "selectPreviousRowExtendSelection",
                "shift KP_UP", "selectPreviousRowExtendSelection",
                "PAGE_UP", "scrollUpChangeSelection",
                "PAGE_DOWN", "scrollDownChangeSelection",
                "HOME", "selectFirstColumn",
                "END", "selectLastColumn",
                "shift PAGE_UP", "scrollUpExtendSelection",
                "shift PAGE_DOWN", "scrollDownExtendSelection",
                "shift HOME", "selectFirstColumnExtendSelection",
                "shift END", "selectLastColumnExtendSelection",
                "ctrl PAGE_UP", "scrollLeftChangeSelection",
                "ctrl PAGE_DOWN", "scrollRightChangeSelection",
                "ctrl HOME", "selectFirstRow",
                "ctrl END", "selectLastRow",
                "ctrl shift PAGE_UP", "scrollRightExtendSelection",
                "ctrl shift PAGE_DOWN", "scrollLeftExtendSelection",
                "ctrl shift HOME", "selectFirstRowExtendSelection",
                "ctrl shift END", "selectLastRowExtendSelection",
                "TAB", "selectNextColumnCell",
                "shift TAB", "selectPreviousColumnCell",
                "ENTER", "selectNextRowCell",
                "shift ENTER", "selectPreviousRowCell",
                "ctrl A", "selectAll",
                "meta A", "selectAll",
                "ESCAPE", "cancel",
                "F2", "startEditing"
            }));
    }
}
