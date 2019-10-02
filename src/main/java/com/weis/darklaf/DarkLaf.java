package com.weis.darklaf;

import com.weis.darklaf.platform.windows.JNIDecorations;
import com.weis.darklaf.ui.menu.DarkPopupMenuUI;
import com.weis.darklaf.util.LafUtil;
import com.weis.darklaf.util.SystemInfo;
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
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * CustomDarcula Look and Feel to allow for better extension.
 */
public class DarkLaf extends BasicLookAndFeel {

    private static final Logger LOGGER = Logger.getLogger(DarkLaf.class.getName());
    private static final String NAME = "Darklaf";
    private static boolean decorationsEnabled = true;
    private BasicLookAndFeel base;

    /**
     * Create Custom Darcula LaF.
     */
    public DarkLaf() {
        LafManager.setCurrentLaf(LafManager.Theme.Dark);
        try {
            if (SystemInfo.isWindows || SystemInfo.isLinux) {
                base = new MetalLookAndFeel();
                MetalLookAndFeel.setCurrentTheme(new DarkMetalTheme());
            } else {
                final String name = UIManager.getSystemLookAndFeelClassName();
                base = (BasicLookAndFeel) Class.forName(name).getDeclaredConstructor().newInstance();
            }
        } catch (@NotNull final Exception e) {
            LOGGER.log(Level.SEVERE, e.toString(), e);
        }
    }

    public static void loadCustomProperties(@NotNull final Properties properties) {
        for (final String key : properties.stringPropertyNames()) {
            UIManager.put(key, LafUtil.parseValue(key, properties.getProperty(key)));
        }
    }

    private static void patchComboBox(@NotNull final UIDefaults metalDefaults, @NotNull final UIDefaults defaults) {
        defaults.remove("ComboBox.ancestorInputMap");
        defaults.remove("ComboBox.actionMap");
        defaults.put("ComboBox.ancestorInputMap", metalDefaults.get("ComboBox.ancestorInputMap"));
        defaults.put("ComboBox.actionMap", metalDefaults.get("ComboBox.actionMap"));
    }

    private static void loadStyleSheet() {
        StyleSheet defaultStyles = new StyleSheet();
        try (Reader r = new BufferedReader(
                new InputStreamReader(DarkLaf.class.getResourceAsStream("darcula.css"),
                                      StandardCharsets.UTF_8))) {
            defaultStyles.loadRules(r, null);
            new HTMLEditorKit().setStyleSheet(defaultStyles);
        } catch (Throwable e) {
            LOGGER.severe(e.getMessage());
        }
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

    @Contract(pure = true)
    public static boolean isDecorationsEnabled() {
        return decorationsEnabled;
    }

    public static void setDecorationsEnabled(boolean decorationsEnabled) {
        if (DarkLaf.decorationsEnabled != decorationsEnabled) {
            DarkLaf.decorationsEnabled = decorationsEnabled;
            boolean update = JNIDecorations.updateLibrary();
            if (update) {
                LafManager.updateLaf();
            }
        }
    }

    @NotNull
    public String getPrefix() {
        return "darcula";
    }

    private void callInit(@NotNull final String method, final UIDefaults defaults) {
        try {
            final Method superMethod = BasicLookAndFeel.class.getDeclaredMethod(method, UIDefaults.class);
            superMethod.setAccessible(true);
            superMethod.invoke(base, defaults);
        } catch (@NotNull final Exception e) {
            LOGGER.log(Level.SEVERE, e.toString(), e);
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
            initIdeaDefaults(defaults);
            patchComboBox(metalDefaults, defaults);
            loadStyleSheet();
            JFrame.setDefaultLookAndFeelDecorated(true);
            JDialog.setDefaultLookAndFeelDecorated(true);

            if (SystemInfo.isMac
                && !"true".equalsIgnoreCase(System.getProperty("apple.laf.useScreenMenuBar", "false"))) {
                //Todo.
                defaults.put("MenuBarUI", "com.bulenkov.darcula.ui.DarculaMenuBarUI");
                defaults.put("MenuUI", "javax.swing.plaf.basic.BasicMenuUI");
            }
            return defaults;
        } catch (@NotNull final Exception e) {
            LOGGER.log(Level.SEVERE, e.toString(), e);
        }
        return super.getDefaults();
    }

    private void loadDefaults(@NotNull final UIDefaults defaults) {
        Properties properties = LafUtil.loadProperties(this);

        final HashMap<String, Object> darculaGlobalSettings = new HashMap<>();
        final String prefix = getPrefix() + '.';
        for (final String key : properties.stringPropertyNames()) {
            if (key.startsWith(prefix)) {
                darculaGlobalSettings
                        .put(key.substring(prefix.length()), LafUtil.parseValue(key, properties.getProperty(key)));
            }
        }

        for (final Object key : defaults.keySet()) {
            if (key instanceof String && ((String) key).contains(".")) {
                final String s = (String) key;
                final String darculaKey = s.substring(s.lastIndexOf('.') + 1);
                if (darculaGlobalSettings.containsKey(darculaKey)) {
                    defaults.put(key, darculaGlobalSettings.get(darculaKey));
                }
            }
        }

        for (final String key : properties.stringPropertyNames()) {
            final String value = properties.getProperty(key);
            defaults.put(key, LafUtil.parseValue(key, value));
        }
    }

    @NotNull
    @Override
    public String getName() {
        return NAME;
    }

    @NotNull
    @Override
    public String getID() {
        return getName();
    }

    @NotNull
    @Override
    public String getDescription() {
        return "Dark Look and feel based on Darcula-LAF";
    }

    @Override
    public boolean isNativeLookAndFeel() {
        return true;
    }

    @Override
    public boolean isSupportedLookAndFeel() {
        return true;
    }

    @Override
    protected void initSystemColorDefaults(final UIDefaults defaults) {
        callInit("initSystemColorDefaults", defaults);
    }

    @Override
    protected void initClassDefaults(final UIDefaults defaults) {
        callInit("initClassDefaults", defaults);
    }

    public void initComponentDefaults(final UIDefaults defaults) {
        callInit("initComponentDefaults", defaults);
    }


    @Override
    public void initialize() {
        call("initialize");
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

    private void call(@NotNull final String method) {
        try {
            final Method superMethod = BasicLookAndFeel.class.getDeclaredMethod(method);
            superMethod.setAccessible(true);
            superMethod.invoke(base);
        } catch (@NotNull final Exception e) {
            LOGGER.log(Level.SEVERE, e.toString(), e);
        }
    }

    @Override
    protected void loadSystemColors(final UIDefaults defaults, final String[] systemColors, final boolean useNative) {
        try {
            final Method superMethod = BasicLookAndFeel.class.getDeclaredMethod(
                    "loadSystemColors", UIDefaults.class, String[].class, boolean.class);
            superMethod.setAccessible(true);
            superMethod.invoke(base, defaults, systemColors, useNative);
        } catch (@NotNull final Exception e) {
            LOGGER.log(Level.SEVERE, e.toString(), e);
        }
    }

    @Override
    public boolean getSupportsWindowDecorations() {
        return JNIDecorations.isCustomDecorationSupported();
    }

    @SuppressWarnings({"HardCodedStringLiteral"})
    private void initIdeaDefaults(@NotNull final UIDefaults defaults) {
        loadDefaults(defaults);
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
                        // "ENTER", "selectNextRowCell",
                        "shift ENTER", "selectPreviousRowCell",
                        "ctrl A", "selectAll",
                        "meta A", "selectAll",
                        // "ESCAPE", "cancel",
                        "F2", "startEditing"
                }));
    }

}
