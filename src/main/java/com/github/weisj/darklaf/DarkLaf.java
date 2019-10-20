/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.github.weisj.darklaf;

import com.github.weisj.darklaf.platform.windows.JNIDecorations;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.ui.menu.DarkPopupMenuUI;
import com.github.weisj.darklaf.util.PropertyLoader;
import com.github.weisj.darklaf.util.SystemInfo;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import sun.awt.AppContext;

import javax.swing.*;
import javax.swing.plaf.basic.BasicLookAndFeel;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Jannis Weis
 */
public class DarkLaf extends BasicLookAndFeel implements PropertyChangeListener {


    private static final Logger LOGGER = Logger.getLogger(DarkLaf.class.getName());
    private static final String NAME = "Darklaf";
    private static boolean decorationsEnabled = true;
    private final BasicLookAndFeel base;

    /**
     * Create Custom Darcula LaF.
     */
    public DarkLaf() {
        try {
            LafManager.getTheme().beforeInstall();
            if (SystemInfo.isWindows || SystemInfo.isLinux) {
                base = new MetalLookAndFeel();
            } else {
                final String name = UIManager.getSystemLookAndFeelClassName();
                base = (BasicLookAndFeel) Class.forName(name).getDeclaredConstructor().newInstance();
            }
        } catch (@NotNull final Exception e) {
            LOGGER.log(Level.SEVERE, e.getMessage(), e.getStackTrace());
            throw new IllegalStateException("Could not load base LaF class." + e.getMessage());
        }
    }

    @Contract(pure = true)
    public static boolean isDecorationsEnabled() {
        return LafManager.getTheme().useCustomDecorations() && decorationsEnabled;
    }

    public static void setDecorationsEnabled(final boolean decorationsEnabled) {
        if (DarkLaf.decorationsEnabled != decorationsEnabled) {
            DarkLaf.decorationsEnabled = decorationsEnabled;
            if (decorationsEnabled) {
                boolean update = JNIDecorations.updateLibrary();
                if (update) {
                    LafManager.updateLaf();
                }
            }
        }
    }

    @Override
    public UIDefaults getDefaults() {
        try {
            final Method superMethod = BasicLookAndFeel.class.getDeclaredMethod("getDefaults");
            superMethod.setAccessible(true);
            final UIDefaults metalDefaults = (UIDefaults) superMethod.invoke(new MetalLookAndFeel());
            final UIDefaults defaults = (UIDefaults) superMethod.invoke(base);

            initInputMapDefaults(defaults);
            loadThemeDefaults(defaults);
            initIdeaDefaults(defaults);
            patchComboBox(metalDefaults, defaults);

            JFrame.setDefaultLookAndFeelDecorated(true);
            JDialog.setDefaultLookAndFeelDecorated(true);

            if (SystemInfo.isMac && !"true".equalsIgnoreCase(System.getProperty("apple.laf.useScreenMenuBar",
                                                                                "false"))) {
                //Todo.
                defaults.put("MenuBarUI", "com.weis.darklaf.ui.menu.DarkMenuBarUI");
                defaults.put("MenuUI", "javax.swing.plaf.basic.BasicMenuUI");
            }
            return defaults;
        } catch (@NotNull final Exception e) {
            LOGGER.log(Level.SEVERE, e.toString(), e.getStackTrace());
        }
        return super.getDefaults();
    }

    @NotNull
    @Override
    public String getName() {
        return NAME;
    }

    @SuppressWarnings({"HardCodedStringLiteral"})
    private static void initInputMapDefaults(@NotNull final UIDefaults defaults) {
        // Make ENTER work in JTrees
        final InputMap treeInputMap = (InputMap) defaults.get("Tree.focusInputMap");
        if (treeInputMap != null) {
            // it's really possible. For example,  GTK+ doesn't have such map
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
    }

    private void loadThemeDefaults(@NotNull final UIDefaults defaults) {
        var uiProps = new Properties();
        final Theme currentTheme = LafManager.getTheme();
        currentTheme.loadDefaults(uiProps, defaults);
        currentTheme.loadGlobals(uiProps, defaults);
        installGlobals(uiProps, defaults);
        currentTheme.loadUIProperties(uiProps, defaults);
        currentTheme.loadPlatformProperties(uiProps, defaults);
        defaults.putAll(uiProps);

        StyleSheet styleSheet = currentTheme.loadStyleSheet();
        new HTMLEditorKit().setStyleSheet(styleSheet);
        setDecorationsEnabled(currentTheme.useCustomDecorations());
    }

    @SuppressWarnings({"HardCodedStringLiteral"})
    private void initIdeaDefaults(@NotNull final UIDefaults defaults) {
        defaults.put("Table.ancestorInputMap", new UIDefaults.LazyInputMap(
                new Object[]{
                        "ctrl C", "copy",
                        "ctrl V", "paste",
                        "ctrl X", "cut",
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

    @NotNull
    @Override
    public String getID() {
        return getName();
    }

    private static void patchComboBox(@NotNull final UIDefaults metalDefaults, @NotNull final UIDefaults defaults) {
        defaults.remove("ComboBox.ancestorInputMap");
        defaults.remove("ComboBox.actionMap");
        defaults.put("ComboBox.ancestorInputMap", metalDefaults.get("ComboBox.ancestorInputMap"));
        defaults.put("ComboBox.actionMap", metalDefaults.get("ComboBox.actionMap"));
    }

    private static void installCutCopyPasteShortcuts(@NotNull final InputMap inputMap,
                                                     final boolean useSimpleActionKeys) {
        final String copyActionKey = useSimpleActionKeys ? "copy" : DefaultEditorKit.copyAction;
        final String pasteActionKey = useSimpleActionKeys ? "paste" : DefaultEditorKit.pasteAction;
        final String cutActionKey = useSimpleActionKeys ? "cut" : DefaultEditorKit.cutAction;
        // Ctrl+Ins, Shift+Ins, Shift+Del
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, InputEvent.CTRL_DOWN_MASK), copyActionKey);
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, InputEvent.SHIFT_DOWN_MASK), pasteActionKey);
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, InputEvent.SHIFT_DOWN_MASK), cutActionKey);
        // Ctrl+C, Ctrl+V, Ctrl+X
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_C, InputEvent.CTRL_DOWN_MASK), copyActionKey);
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_V, InputEvent.CTRL_DOWN_MASK), pasteActionKey);
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_X, InputEvent.CTRL_DOWN_MASK), DefaultEditorKit.cutAction);
    }

    private void installGlobals(@NotNull final Properties uiProps, final UIDefaults defaults) {
        final HashMap<String, Object> globalSettings = new HashMap<>();
        final String prefix = "global.";
        for (final Object key : uiProps.keySet()) {
            if (key instanceof String && ((String) key).startsWith(prefix)) {
                globalSettings.put(((String) key).substring(prefix.length()), uiProps.get(key));
            }
        }

        for (final Object key : defaults.keySet()) {
            if (key instanceof String && ((String) key).contains(".")) {
                final String s = (String) key;
                final String globalKey = s.substring(s.lastIndexOf('.') + 1);
                if (globalSettings.containsKey(globalKey)) {
                    defaults.put(key, globalSettings.get(globalKey));
                }
            }
        }
    }

    @NotNull
    @Override
    public String getDescription() {
        return "Dark Look and feel based on Darcula-LAF";
    }

    @Override
    public void initialize() {
        call("initialize");
        PropertyLoader.reset();
        UIManager.addPropertyChangeListener(this);
    }

    @Override
    public void uninitialize() {
        call("uninitialize");
        AppContext context = AppContext.getAppContext();
        synchronized (DarkPopupMenuUI.MOUSE_GRABBER_KEY) {
            Object grabber = context.get(DarkPopupMenuUI.MOUSE_GRABBER_KEY);
            if (grabber != null) {
                ((DarkPopupMenuUI.MouseGrabber) grabber).uninstall();
            }
        }
    }

    @Override
    protected void initClassDefaults(final UIDefaults defaults) {
        callInit("initClassDefaults", defaults);
    }

    @Override
    public boolean isNativeLookAndFeel() {
        return true;
    }

    @Override
    protected void initSystemColorDefaults(final UIDefaults defaults) {
        callInit("initSystemColorDefaults", defaults);
    }

    @Override
    protected void loadSystemColors(final UIDefaults defaults, final String[] systemColors,
                                    final boolean useNative) {
        try {
            final Method superMethod = BasicLookAndFeel.class.getDeclaredMethod(
                    "loadSystemColors", UIDefaults.class, String[].class, boolean.class);
            superMethod.setAccessible(true);
            superMethod.invoke(base, defaults, systemColors, useNative);
        } catch (@NotNull final Exception e) {
            LOGGER.log(Level.SEVERE, e.toString(), e.getStackTrace());
        }
    }

    public void initComponentDefaults(final UIDefaults defaults) {
        callInit("initComponentDefaults", defaults);
    }

    @Override
    public boolean isSupportedLookAndFeel() {
        return true;
    }

    private void callInit(@NotNull final String method, final UIDefaults defaults) {
        try {
            final Method superMethod = BasicLookAndFeel.class.getDeclaredMethod(method, UIDefaults.class);
            superMethod.setAccessible(true);
            superMethod.invoke(base, defaults);
        } catch (@NotNull final Exception e) {
            LOGGER.log(Level.SEVERE, e.toString(), e.getStackTrace());
        }
    }

    private void call(@NotNull final String method) {
        try {
            final Method superMethod = BasicLookAndFeel.class.getDeclaredMethod(method);
            superMethod.setAccessible(true);
            superMethod.invoke(base);
        } catch (@NotNull final Exception e) {
            LOGGER.log(Level.SEVERE, e.toString(), e.getStackTrace());
        }
    }

    @Override
    public void propertyChange(@NotNull final PropertyChangeEvent evt) {
        if ("lookAndFeel".equals(evt.getPropertyName())) {
            if (UIManager.getLookAndFeel() == this) {
                PropertyLoader.finish();
            }
            UIManager.removePropertyChangeListener(this);
        }
    }


    @Override
    public boolean getSupportsWindowDecorations() {
        return LafManager.getTheme().useCustomDecorations()
                && JNIDecorations.isCustomDecorationSupported();
    }


}
