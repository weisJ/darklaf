package com.github.weisj.darklaf.ui.demo;

import java.awt.Dimension;
import java.awt.Window;
import java.util.Collections;
import java.util.List;

import javax.swing.Icon;
import javax.swing.JMenu;

import com.github.weisj.darklaf.theme.Theme;

public interface DemoExecutionSpec {

    default Theme getTheme() {
        return DemoExecutor.getPreferredTheme();
    }

    default boolean useDarklaf() {
        return true;
    }

    default Dimension getWindowSize() {
        return null;
    }

    default Icon getFrameIcon() {
        return null;
    }

    default List<JMenu> createMenus() {
        return Collections.emptyList();
    }

    default void configureWindow(final Window window) {}

    default boolean hasMenuBar() {
        return true;
    }
}
