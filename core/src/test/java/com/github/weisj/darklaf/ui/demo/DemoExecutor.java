/*
 * MIT License
 *
 * Copyright (c) 2021-2022 Jannis Weis
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
package com.github.weisj.darklaf.ui.demo;

import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.Window;
import java.lang.reflect.InvocationTargetException;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.Icon;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import org.jetbrains.annotations.NotNull;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.platform.SystemInfo;
import com.github.weisj.darklaf.properties.icons.IconLoader;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.spec.PreferredThemeStyle;
import com.github.weisj.swingdsl.inspector.InspectorKt;

public final class DemoExecutor {

    public static boolean isRunningOnModulePath() {
        if (SystemInfo.isJava9OrGreater) {
            try {
                Object module = Class.class.getMethod("getModule").invoke(DarkLaf.class);
                Class<?> moduleClass = Class.forName("java.lang.Module");
                return Boolean.TRUE.equals(moduleClass.getMethod("isNamed").invoke(module));
            } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException
                    | ClassNotFoundException e) {
                return false;
            }
        }
        return false;
    }

    public static Theme getPreferredTheme() {
        PreferredThemeStyle themeStyle = LafManager.getPreferredThemeStyle();
        return LafManager.themeForPreferredStyle(
                new PreferredThemeStyle(themeStyle.getContrastRule(), themeStyle.getColorToneRule()));
    }

    public static void showDemo(final ComponentDemo demo) {
        showDemo(demo, false);
    }

    public static AtomicReference<Window> showDemo(final ComponentDemo demo, final boolean asDialog) {
        InspectorKt.installInspector();
        LafManager.enabledPreferenceChangeReporting(false);
        LafManager.addThemePreferenceChangeListener(LafManager::installTheme);
        return showDemoWithoutSetup(demo, asDialog);
    }

    public static AtomicReference<Window> showDemoWithoutSetup(final ComponentDemo demo, final boolean asDialog) {
        AtomicReference<Window> windowRef = new AtomicReference<>();
        DemoExecutionSpec executionSpec = demo.getExecutionSpec();

        if (SystemInfo.isJava9OrGreater && !isRunningOnModulePath()) {
            throw new IllegalStateException("Not running on module path");
        }

        Runnable demoRunnable = () -> {
            try {
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
                }
            } finally {
                synchronized (windowRef) {
                    windowRef.notifyAll();
                }
            }
        };

        if (SwingUtilities.isEventDispatchThread()) {
            demoRunnable.run();
        } else {
            SwingUtilities.invokeLater(demoRunnable);
            synchronized (windowRef) {
                try {
                    windowRef.wait();
                } catch (InterruptedException e) {
                    throw new IllegalStateException(e);
                }
            }
        }

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
