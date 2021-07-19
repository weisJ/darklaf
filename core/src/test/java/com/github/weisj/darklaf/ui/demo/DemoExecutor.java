package com.github.weisj.darklaf.ui.demo;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.properties.icons.IconLoader;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.info.PreferredThemeStyle;
import org.jetbrains.annotations.NotNull;

import javax.swing.Icon;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.Window;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Level;

public final class DemoExecutor {

    public static Theme getPreferredTheme() {
        PreferredThemeStyle themeStyle = LafManager.getPreferredThemeStyle();
        return LafManager.themeForPreferredStyle(
            new PreferredThemeStyle(themeStyle.getContrastRule(), themeStyle.getColorToneRule()));
    }

    public static void showDemo(final ComponentDemo demo) {
        showDemo(demo, false);
    }

    public static AtomicReference<Window> showDemo(final ComponentDemo demo, final boolean asDialog,
            final Level logLevel) {
        LafManager.setLogLevel(logLevel != null ? logLevel : Level.FINE);
        return showDemo(demo, asDialog);
    }

    public static AtomicReference<Window> showDemo(final ComponentDemo demo, final boolean asDialog) {
        LafManager.enabledPreferenceChangeReporting(false);
        LafManager.addThemePreferenceChangeListener(LafManager::installTheme);
        LafManager.setDecorationsEnabled(true);
        AtomicReference<Window> windowRef = new AtomicReference<>();
        DemoExecutionSpec executionSpec = demo.getExecutionSpec();

        SwingUtilities.invokeLater(() -> {
            setupLaf(executionSpec);
            Window window = createWindow(demo, asDialog);
            executionSpec.configureWindow(window);

            Icon icon = executionSpec.getFrameIcon();
            if (icon != null) {
                window.setIconImage(IconLoader.createFrameIcon(icon, window));
            }

            window.pack();
            configureWindowSize(executionSpec, window);

            window.setVisible(true);
            window.setLocationRelativeTo(null);
            synchronized (windowRef) {
                windowRef.set(window);
                windowRef.notifyAll();
            }
        });
        return windowRef;
    }

    @NotNull
    private static Window createWindow(final ComponentDemo demo, final boolean asDialog) {
        Window window;
        if (!asDialog) {
            JFrame frame = new JFrame();
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setTitle(demo.getName());
            frame.setContentPane(demo.getContentPane());
            if (demo.getExecutionSpec().hasMenuBar()) {
                frame.setJMenuBar(createMenuBar(demo));
            }
            window = frame;
        } else {
            JDialog dialog = new JDialog();
            dialog.setModalityType(Dialog.ModalityType.MODELESS);
            dialog.setTitle(demo.getName());
            dialog.setContentPane(demo.getContentPane());
            if (demo.getExecutionSpec().hasMenuBar()) {
                dialog.setJMenuBar(createMenuBar(demo));
            }
            window = dialog;
        }
        return window;
    }

    private static JMenuBar createMenuBar(final ComponentDemo demo) {
        JMenuBar menuBar = new DemoMenuBar();
        demo.getExecutionSpec().createMenus().forEach(menuBar::add);
        return menuBar;
    }

    private static void configureWindowSize(DemoExecutionSpec executionSpec, Window window) {
        Dimension dimension = executionSpec.getWindowSize();
        if (dimension == null) {
            Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
            Dimension dim = new Dimension(screenSize.width / 2, screenSize.height / 2);
            Dimension targetSize = window.getSize();
            targetSize.width = Math.min(targetSize.width, dim.width);
            targetSize.height = Math.min(targetSize.height, dim.height);
            window.setSize(targetSize);
        } else {
            window.setSize(dimension);
        }
    }

    private static void setupLaf(DemoExecutionSpec executionSpec) {
        if (executionSpec.useDarklaf()) {
            if (!LafManager.isInstalled()) {
                LafManager.install(executionSpec.getTheme());
            }
        } else {
            installSystemLaf();
        }
    }

    private static void installSystemLaf() {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            LafManager.updateLaf();
        } catch (ClassNotFoundException | UnsupportedLookAndFeelException | IllegalAccessException
                | InstantiationException classNotFoundException) {
            classNotFoundException.printStackTrace();
        }
    }
}
