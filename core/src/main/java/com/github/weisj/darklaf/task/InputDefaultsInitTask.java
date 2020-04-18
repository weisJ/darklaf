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
 *
 */
package com.github.weisj.darklaf.task;

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

import javax.swing.*;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.text.DefaultEditorKit;

import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.util.SystemInfo;

public class InputDefaultsInitTask implements DefaultsInitTask {

    @Override
    public void run(final Theme currentTheme, final UIDefaults defaults) {
        initInputMapDefaults(defaults);
        patchComboBox(new MetalLookAndFeel().getDefaults(), defaults);
    }

    private void initInputMapDefaults(final UIDefaults defaults) {
        // Make ENTER work in JTrees
        final InputMap treeInputMap = (InputMap) defaults.get("Tree.focusInputMap");
        if (treeInputMap != null) {
            // it's really possible. For example, GTK+ doesn't have such a map.
            treeInputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "toggle");
        }
        // Cut/Copy/Paste in JTextAreas
        final InputMap textAreaInputMap = (InputMap) defaults.get("TextArea.focusInputMap");
        if (textAreaInputMap != null) {
            // It really can be null, for example when LAF isn't properly initialized
            // (Alloy license problem)
            installCutCopyPasteShortcuts(textAreaInputMap, false);
        }
        // Cut/Copy/Paste in JTextFields
        final InputMap textFieldInputMap = (InputMap) defaults.get("TextField.focusInputMap");
        if (textFieldInputMap != null) {
            // It really can be null, for example when LAF isn't properly initialized
            // (Alloy license problem)
            installCutCopyPasteShortcuts(textFieldInputMap, false);
        }
        // Cut/Copy/Paste in JPasswordField
        final InputMap passwordFieldInputMap = (InputMap) defaults.get("PasswordField.focusInputMap");
        if (passwordFieldInputMap != null) {
            // It really can be null, for example when LAF isn't properly initialized
            // (Alloy license problem)
            installCutCopyPasteShortcuts(passwordFieldInputMap, false);
        }
        // Cut/Copy/Paste in JTables
        final InputMap tableInputMap = (InputMap) defaults.get("Table.ancestorInputMap");
        if (tableInputMap != null) {
            // It really can be null, for example when LAF isn't properly initialized
            // (Alloy license problem)
            installCutCopyPasteShortcuts(tableInputMap, true);
        }
        final InputMap buttonInputMap = (InputMap) defaults.get("Button.focusInputMap");
        if (buttonInputMap != null && !SystemInfo.isMac) {
            buttonInputMap.put(KeyStroke.getKeyStroke("ENTER"), "pressed");
            buttonInputMap.put(KeyStroke.getKeyStroke("released ENTER"), "released");
        }
    }

    private void installCutCopyPasteShortcuts(final InputMap inputMap,
                                              final boolean useSimpleActionKeys) {
        final String copyActionKey = useSimpleActionKeys ? "copy" : DefaultEditorKit.copyAction;
        final String pasteActionKey = useSimpleActionKeys ? "paste" : DefaultEditorKit.pasteAction;
        final String cutActionKey = useSimpleActionKeys ? "cut" : DefaultEditorKit.cutAction;
        final int mask = SystemInfo.isMac ? InputEvent.META_DOWN_MASK : InputEvent.CTRL_DOWN_MASK;
        // Ctrl+Ins, Shift+Ins, Shift+Del
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, InputEvent.CTRL_DOWN_MASK), copyActionKey);
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, InputEvent.SHIFT_DOWN_MASK), pasteActionKey);
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, InputEvent.SHIFT_DOWN_MASK), cutActionKey);
        // Ctrl+C, Ctrl+V, Ctrl+X
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_C, mask), copyActionKey);
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_V, mask), pasteActionKey);
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_X, mask), DefaultEditorKit.cutAction);
    }

    private static void patchComboBox(final UIDefaults metalDefaults, final UIDefaults defaults) {
        defaults.remove("ComboBox.ancestorInputMap");
        defaults.remove("ComboBox.actionMap");
        defaults.put("ComboBox.ancestorInputMap", metalDefaults.get("ComboBox.ancestorInputMap"));
        defaults.put("ComboBox.actionMap", metalDefaults.get("ComboBox.actionMap"));
    }
}
